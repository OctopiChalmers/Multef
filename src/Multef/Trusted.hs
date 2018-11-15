-- | This module should _only_ be imported by trusted code
module Multef.Trusted 
  ( module Faceted
  , printResult
  )
where

import Data.IORef

import Faceted
import FIOInternals

instance (Show a, Show l) => Show (Faceted l a) where
  show (Raw v) = show v
  show (Facet l fl fr) = "< " ++ show l ++ " ? " ++ show fl ++ " : " ++ show fr ++ " >"

printRuntime :: FIORuntimeObject -> IO ()
printRuntime obj = do
  act <- readIORef (activeThreadCount obj)
  max <- readIORef (totalForkCount obj)
  putStrLn $ "Active #Threads: " ++ show act
  putStrLn $ "Total #Threads: " ++ show max

printResult :: (Show l, Show a) => FIOExecutionObject l a -> IO ()
printResult obj = do
  res <- readIORef (result obj) 
  printRuntime (runtime obj)
  putStrLn $ show res
