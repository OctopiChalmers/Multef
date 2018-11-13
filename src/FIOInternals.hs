{-# LANGUAGE GADTs #-}
module FIOInternals 
  ( FIO
  , FIOExecutionObject(..)
  , fsme
  , control
  , readFIORef
  , writeFIORef
  , newFIORef
  )
where

import Data.IORef
import Control.Monad
import Control.Concurrent
import System.Timeout

import Lattice
import ProgramCounter
import Faceted

-- | A reference holding a faceted value
-- with labels drawn from `l`
newtype FIORef l a = FIORef (IORef (Faceted l a))

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

-- | What is returned when executing an FIO program
data FIOExecutionObject a =
  FIOExec { result            :: a
          , activeThreadCount :: IORef Int }

-- | Execute an `FIO` program under FSME. The `waitTime` parameter
--   specifies the timeout
fsme :: Lattice l => FIO l a -> PC l -> Int -> IO (FIOExecutionObject a)
fsme fio pc waitTime = do
  activeThreads <- newIORef 1
  (res, _)      <- run activeThreads fio pc
  atomicModifyIORef activeThreads $ \x -> (x-1, ())
  return $ FIOExec { result            = res
                   , activeThreadCount = activeThreads }
  where
    run :: Lattice l => IORef Int -> FIO l a -> PC l -> IO (a, PC l)
    run act fio pc = case fio of
      Done a -> return (a, pc)

      Control (Raw fioa) cont -> do
        (a, _) <- run act fioa pc
        run act (cont (Raw a)) pc

      Control (Facet l prv pub) cont
        | emptyView (extendPositive pc l) -> run act (Control pub cont) (extendPositive pc l)
        | emptyView (extendNegative pc l) -> run act (Control prv cont) (extendNegative pc l)
        | otherwise -> do
            -- For communication between the two threads
            privResultMVar <- newEmptyMVar
            privCont       <- newEmptyMVar
            -- We are spawning a new thread
            atomicModifyIORef act $ \x -> (x + 1, ())
            forkIO $ do
              -- Run the private computation
              (result, pc') <- run act (Control prv Done) (extendPositive pc l) 
              -- Communicate the result to the other thread
              putMVar privResultMVar result 
              -- Check if we should switch to SME
              switchSME <- readMVar privCont 
              -- If we switch, continue by running the continuation
              when switchSME . void $ run act (cont result) pc'
              -- Clean up after ourselves
              atomicModifyIORef act $ \x -> (x - 1, ())

            -- Check if we have a result yet
            onTime <- timeout waitTime (readMVar privResultMVar)

            case onTime of
              -- Continue under MF
              Just privResult -> do
                putMVar privCont False
                (pubResult, _) <- run act (Control pub Done) (extendNegative pc l)
                run act (cont (Facet l privResult pubResult)) pc

              -- Switching to SME-like semantics
              Nothing -> do
                putMVar privCont True
                run act (Control pub cont) (extendNegative pc l)

      ReadFIORef (FIORef ref) cont -> do
        fac <- readIORef ref
        run act (cont fac) pc

      WriteFIORef (FIORef ref) fac cont -> do
        -- `pcF pc` means we include information from the
        -- existing context in the reference cell
        atomicModifyIORef' ref $ \old -> (pcF pc fac old, ())
        run act cont pc

      NewFIORef fac cont -> do
        ref <- FIORef <$> newIORef fac
        run act (cont ref) pc

instance Monad (FIO l) where
  return    = Done

  fio >>= k = case fio of
    Done a                  -> k a

    Control fac cont        -> Control fac    (cont >=> k)

    ReadFIORef ref cont     -> ReadFIORef ref (cont >=> k) 

    WriteFIORef ref fac fio -> WriteFIORef ref fac (fio >>= k)

    NewFIORef fac cont      -> NewFIORef fac (cont >=> k)

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

-- | Creat a new reference with an initial value
newFIORef :: Faceted l a -> FIO l (FIORef l a)
newFIORef fac = NewFIORef fac Done
