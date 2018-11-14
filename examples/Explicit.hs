module Explicit where

import Multef

leak :: Lattice l => Faceted l Bool -> FIO l (Faceted l Bool)
leak x = do
  y <- newFIORef (pure False)
  writeFIORef y x
  readFIORef y
