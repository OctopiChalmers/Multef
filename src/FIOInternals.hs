{-# LANGUAGE GADTs #-}
module FIOInternals
  ( FIO(..)
  )
where

import Data.IORef
import Control.Monad
import Control.Concurrent
import System.Timeout

import Lattice
import ProgramCounter
import Faceted

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

fsme :: Lattice l => FIO l a -> PC l -> Int -> IO a
fsme fio pc waitTime = fst <$> run fio pc
  where
    run :: Lattice l => FIO l a -> PC l -> IO (a, PC l)
    run fio pc = case fio of
      Done a -> return (a, pc)

      Control (Raw fioa) cont -> do
        (a, _) <- run fioa pc
        run (cont (Raw a)) pc

      Control (Facet l prv pub) cont
        | emptyView (extendPositive pc l) -> run (Control pub cont) (extendPositive pc l)
        | emptyView (extendNegative pc l) -> run (Control prv cont) (extendNegative pc l)
        | otherwise -> do
            privResultMVar <- newEmptyMVar
            privCont       <- newEmptyMVar
            forkIO $ do
              -- Run the private computation
              (result, pc') <- run (Control prv Done) (extendPositive pc l) 
              -- Communicate the result to the other thread
              putMVar privResultMVar result 
              -- Check if we should switch to SME
              switchSME <- readMVar privCont 
              -- If we switch, continue by running the continuation
              when switchSME . void $ run (cont result) pc'

            -- Check if we have a result yet
            onTime <- timeout waitTime (readMVar privResultMVar)

            case onTime of
              Just privResult -> do
                putMVar privCont False
                (pubResult, _) <- run (Control pub Done) (extendNegative pc l)
                run (cont (Facet l privResult pubResult)) pc

              Nothing -> do
                -- Switching to SME
                putMVar privCont True
                run (Control pub cont) (extendNegative pc l)

      ReadFIORef (FIORef ref) cont -> do
        fac <- readIORef ref
        run (cont fac) pc

      WriteFIORef (FIORef ref) fac cont -> do
        atomicModifyIORef' ref $
          \old -> (pcF pc fac old, ())
        run cont pc

      NewFIORef fac cont -> do
        ref <- FIORef <$> newIORef fac
        run (cont ref) pc
