module Explicit where

import Multef

-- | Attempt to leak directly
leak :: Lattice l => Faceted l Bool -> FIO l (Faceted l Bool)
leak x = do
  y <- newFIORef (pure False)
  writeFIORef y x
  readFIORef y
