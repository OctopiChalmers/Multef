-- | This module should _only_ be imported by trusted code
module Multef.Trusted 
  ( module Faceted )
where

import Faceted

instance (Show a, Show l) => Show (Faceted l a) where
  show (Raw v) = show v
  show (Facet l fl fr) = "< " ++ show l ++ " ? " ++ show fl ++ " : " ++ show fr ++ " >"
