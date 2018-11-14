module AustinAndFlanagan where

import Multef

-- | Attempt to leak "through" the PC
leak :: Lattice l => Faceted l Bool -> FIO l (Faceted l Bool)
leak x = do
  y <- newFIORef (pure False)
  z <- newFIORef (pure False)
  when' x $ do
    writeFIORef y (pure True)
  yv <- readFIORef y
  when' yv $ do
    writeFIORef z (pure True)
  readFIORef z
