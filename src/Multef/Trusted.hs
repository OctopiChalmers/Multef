-- | This module should _only_ be imported by trusted code
module Multef.Trusted 
  ( module Faceted
  , module FIOInternals
  , printResult
  )
where

import Data.IORef

import Faceted
import FIOInternals

instance (Show a, Show l) => Show (Faceted l a) where
  show (Raw v) = show v
  show (Facet l fl fr) = "< " ++ show l ++ " ? " ++ show fl ++ " : " ++ show fr ++ " >"
  show Bot = "⊥"

printRuntime :: FIORuntimeObject -> IO ()
printRuntime obj = do
  act <- readIORef (activeThreads obj)
  max <- readIORef (totalForkCount obj)
  putStrLn $ "Active #Threads: " ++ show (length act)
  putStrLn $ "Total #Threads: " ++ show max

printResult :: (Show l, Show a) => FIOExecutionObject l a -> IO ()
printResult obj = do
  res <- readIORef (result obj) 
  printRuntime (runtime obj)
  putStrLn $ show (unMaybeFaceted res)
  where
    unMaybeFaceted :: Faceted l (Maybe a) -> Maybe (Faceted l a)
    unMaybeFaceted (Raw a) = Raw <$> a
    unMaybeFaceted (Facet l fl fr) =
      case unMaybeFaceted fl of
        Just fl -> case unMaybeFaceted fr of
          Just fr -> Just (Facet l fl fr)
          Nothing -> Just fl
        Nothing -> unMaybeFaceted fr
    unMaybeFaceted Bot = Nothing
