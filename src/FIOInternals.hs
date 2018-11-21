{-# LANGUAGE GADTs #-}
module FIOInternals 
  ( FIO
  , FIOExecutionObject(..)
  , FIORuntimeObject(..)
  , fsme
  , control
  , readFIORef
  , writeFIORef
  , newFIORef
  )
where

import Data.IORef
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

-- | An FIO output channel on which we can write faceted values
newtype FOChan l a = FOChan (Chan (Faceted l a))

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
              -> Faceted l a
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

-- TODO: Figure out the least-bad way of doing something like this
facGetDup :: Lattice l => PC l -> Faceted l (Chan (Faceted l a)) -> IO (Faceted l a, Faceted l (Chan (Faceted l a)))
facGetDup pc Bot               = return (Bot, Bot)
facGetDup pc (Raw ch)          = undefined
facGetDup pc (Faceted l fl fr) = undefined

-- | Execute an `FIO` program under FSME. The `waitTime` parameter
--   specifies the timeout
fsme :: Lattice l => FIO l a -> Maybe Int -> IO (FIOExecutionObject l a)
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
    run :: Lattice l => FIORuntimeObject -> FIO l a -> PC l -> IO (a, PC l)
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

      -- TODO: Figure out the least bad way of implementing this
      ReadFIChan ch cont -> undefined

      WriteFOChan (FOChan ch) val cont -> do
        writeChan ch (pcF pc val Bot)
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
    WriteFOChan fac val fio -> WriteFOChan fac val (fio >>= k)

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
