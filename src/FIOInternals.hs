{-# LANGUAGE GADTs #-}
module FIOInternals 
  ( FIO
  , FIORef
  , FIChan (..)
  , FOChan (..)
  , FIOExecutionObject(..)
  , FIORuntimeObject(..)
  , fsme
  , control
  , readFIORef
  , writeFIORef
  , newFIORef
  , readFIChan
  , writeFOChan
  , debug
  )
where

import Data.IORef
import Data.List
import Control.Monad
import Control.Concurrent hiding (killThread)
import Control.Concurrent.Chan
import Control.Concurrent.Lock
import System.Timeout

import Lattice
import ProgramCounter
import Faceted

-- | A reference holding a faceted value
-- with labels drawn from `l`
newtype FIORef l a = FIORef (IORef (Faceted l a))

-- | An FIO input channel from which faceted values can be read
data FIChan l a = FIChan (IORef (Faceted l (Chan (Faceted l a)))) Lock

-- | An FIO output channel on which we can write an unfaceted value.
-- The receiver is also informed of the PC that wrote the value
data FOChan l a = FOChan l (Chan (PC l, a))

-- | An FIO computation, this is the meat of the
-- Multef framework.
data FIO l a where
  Done        :: a
              -> FIO l a

  Control     :: Faceted l (FIO l a)
              -> (Faceted l a -> FIO l b)
              -> FIO l b

  ReadFIORef  :: FIORef l a
              -> (Faceted l a -> FIO l b)
              -> FIO l b

  WriteFIORef :: FIORef l a
              -> Faceted l a
              -> FIO l b
              -> FIO l b

  NewFIORef   :: Faceted l a
              -> (FIORef l a -> FIO l b)
              -> FIO l b

  ReadFIChan  :: FIChan l a
              -> (Faceted l a -> FIO l b)
              -> FIO l b

  WriteFOChan :: FOChan l a
              -> a
              -> FIO l b
              -> FIO l b

  Debug       :: Show l
              => String
              -> FIO l b
              -> FIO l b

-- | What is carried around at runtime
data FIORuntimeObject =
  FIORuntime { activeThreads  :: IORef [ThreadId]
             , totalForkCount :: IORef Int
             }

-- | What is returned when executing an FIO program
data FIOExecutionObject l a =
  FIOExec { result  :: IORef (Faceted l (Maybe a))
          , runtime :: FIORuntimeObject }

-- | Fork a new thread
forkThread :: FIORuntimeObject -> IO () -> IO ()
forkThread runtime comp = do
  tid <- forkIO comp
  atomicModifyIORef' (activeThreads runtime) $ \tids -> (tid : tids, ())
  atomicModifyIORef' (totalForkCount runtime) $ \x -> (x + 1, ())

-- | Kill a thread, needs to be called from the thread being killed
killThread :: FIORuntimeObject -> IO ()
killThread runtime = do
  tid <- myThreadId
  atomicModifyIORef (activeThreads runtime) $ \tids -> (filter (tid /=) tids, ())

-- | Create a new runtime object
createRuntimeObject :: IO FIORuntimeObject
createRuntimeObject = do
  tid   <- myThreadId
  acttc <- newIORef [tid]
  maxtc <- newIORef 1
  return $ FIORuntime acttc maxtc

-- TODO: Figure out a better way of implementing this
-- * Figure out how to get fewer duplicated channels and threads running forever
-- * Figure out how to express this without deleting specific labels from the PC
facGetDup :: (Lattice l, Eq l) => PC l -> Faceted l (Chan (Faceted l a)) -> IO (Faceted l a, Faceted l (Chan (Faceted l a)))
facGetDup pc Bot               = return (Bot, Bot)
-- Optimization
facGetDup (PC []) (Raw ch)     = do
  val <- readChan ch
  return (val, Raw ch)
facGetDup pc (Raw ch)     = do
  lch <- newChan
  rch <- newChan
  -- Terrible solution, leads to threads leaking all over the place
  forkIO . forever $ do
    val <- readChan ch
    writeChan lch val
    writeChan rch val
  val <- readChan lch
  return (pcF pc val Bot, pcF pc (Raw lch) (Raw rch))
facGetDup (PC pc) (Facet l fl fr) = do
  (v0, p0) <- facGetDup (PC $ pc \\ [Positive l, Negative l]) fl
  (v1, p1) <- facGetDup (PC $ pc \\ [Positive l, Negative l]) fr
  let p0' = if Positive l `elem` pc then fl else p0
  let p1' = if Negative l `elem` pc then fr else p1
  return (facet l v0 v1, facet l p0' p1')

-- | Execute an `FIO` program under FSME. The `waitTime` parameter
--   specifies the timeout
fsme :: (Lattice l, Eq l) => FIO l a -> Maybe Int -> IO (FIOExecutionObject l a)
fsme fio waitTime = do
  -- Keep track of the number of active threads
  runtimeObject <- createRuntimeObject
  -- To put the result in when we are done
  resultRef <- newIORef (pure Nothing)
  -- The computation which communicates the result
  let fio' = fio >>= (writeFIORef (FIORef resultRef) . pure . Just)
  -- Run the computation
  run runtimeObject fio' emptyPC
  -- Clean up after ourselves
  killThread runtimeObject
  -- Return the references
  return $ FIOExec { result  = resultRef
                   , runtime = runtimeObject }
  where
    run :: (Lattice l, Eq l) => FIORuntimeObject -> FIO l a -> PC l -> IO (a, PC l)
    run runtime fio pc = case fio of
      Done a -> return (a, pc)

      Control Bot cont -> run runtime (cont Bot) pc

      Control (Raw fioa) cont -> do
        (a, _) <- run runtime fioa pc
        run runtime (cont (Raw a)) pc

      Control (Facet l prv pub) cont
        | emptyView (extendPositive pc l) -> run runtime (Control pub cont) (extendNegative pc l)
        | emptyView (extendNegative pc l) -> run runtime (Control prv cont) (extendPositive pc l)
        | otherwise -> do
            -- For communication between the two threads
            privResultMVar <- newEmptyMVar
            privCont       <- newEmptyMVar
            -- We are spawning a new thread
            forkThread runtime $ do
              -- Run the private computation
              (result, pc') <- run runtime (Control prv Done) (extendPositive pc l) 
              -- Communicate the result to the other thread
              putMVar privResultMVar result 
              -- Check if we should switch to SME
              switchSME <- readMVar privCont 
              -- If we switch, continue by running the continuation
              when switchSME . void $ run runtime (cont result) pc'
              -- Clean up after ourselves
              killThread runtime

            -- Check if we have a result yet
            onTime <- maybe (Just <$>) timeout waitTime (readMVar privResultMVar)

            case onTime of
              -- Continue under MF
              Just privResult -> do
                putMVar privCont False
                (pubResult, _) <- run runtime (Control pub Done) (extendNegative pc l)
                run runtime (cont (Facet l privResult pubResult)) pc

              -- Switching to SME-like semantics
              Nothing -> do
                putMVar privCont True
                run runtime (Control pub cont) (extendNegative pc l)

      ReadFIORef (FIORef ref) cont -> do
        fac <- readIORef ref
        run runtime (cont fac) pc

      WriteFIORef (FIORef ref) fac cont -> do
        -- `pcF pc` means we include information from the
        -- existing context in the reference cell
        atomicModifyIORef' ref $ \old -> (pcF pc fac old, ())
        run runtime cont pc

      NewFIORef fac cont -> do
        ref <- FIORef <$> newIORef fac
        run runtime (cont ref) pc

      ReadFIChan (FIChan chRef lock) cont -> do
        val <- with lock $ do
          fch <- readIORef chRef
          (val, newfch) <- facGetDup pc fch
          writeIORef chRef newfch
          return val
        run runtime (cont val) pc

      WriteFOChan (FOChan l ch) a cont -> do
        when (l `inViews` pc) $ do
          writeChan ch (pc, a)
        run runtime cont pc

      Debug s cont -> do
        putStrLn $ "DEBUG[" ++ show pc ++ "]: " ++ show s
        run runtime cont pc

instance Monad (FIO l) where
  return    = Done

  fio >>= k = case fio of
    Done a                  -> k a
    Control fac cont        -> Control fac    (cont >=> k)
    ReadFIORef ref cont     -> ReadFIORef ref (cont >=> k) 
    WriteFIORef ref fac fio -> WriteFIORef ref fac (fio >>= k)
    NewFIORef fac cont      -> NewFIORef fac (cont >=> k)
    ReadFIChan fac cont     -> ReadFIChan fac (cont >=> k)
    WriteFOChan fac a fio   -> WriteFOChan fac a (fio >>= k)
    Debug s fio             -> Debug s (fio >>= k)

instance Applicative (FIO l) where
  pure  = return
  (<*>) = ap

instance Functor (FIO l) where
  fmap = liftM

-- | Execute secret-dependent control flow
control :: Faceted l (FIO l a) -> FIO l (Faceted l a)
control fac = Control fac Done

-- | Read a reference
readFIORef :: FIORef l a -> FIO l (Faceted l a)
readFIORef ref = ReadFIORef ref Done

-- | Write to a reference
writeFIORef :: FIORef l a -> Faceted l a -> FIO l ()
writeFIORef ref fac = WriteFIORef ref fac (Done ())

-- | Create a new reference with an initial value
newFIORef :: Faceted l a -> FIO l (FIORef l a)
newFIORef fac = NewFIORef fac Done

-- | Read an input channel
readFIChan :: FIChan l a -> FIO l (Faceted l a)
readFIChan fch = ReadFIChan fch Done

-- | Write to an output channel
writeFOChan :: FOChan l a -> a -> FIO l ()
writeFOChan fch a = WriteFOChan fch a (Done ())

-- | Write to the debug log
debug :: Show l => String -> FIO l ()
debug s = Debug s (Done ())
