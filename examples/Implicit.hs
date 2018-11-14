module Implicit where

import Multef

-- | Attempt to leak by control-flow
leak :: Lattice l => Faceted l Bool -> FIO l (Faceted l Bool)
leak x = do
  y <- newFIORef (pure False)
  when' x $ do
    writeFIORef y x
  readFIORef y
