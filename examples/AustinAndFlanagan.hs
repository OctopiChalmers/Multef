module AustinAndFlanagan where

import Multef

-- | An example from Austing and Flanagan
austinAndFlanagan :: Lattice l => Faceted l Bool -> FIO l (Faceted l Bool)
austinAndFlanagan x = do
  y <- newFIORef (pure False)
  z <- newFIORef (pure False)
  when' x $ writeFIORef y (pure True)
  yv <- readFIORef y
  when' yv $ writeFIORef z (pure True)
  readFIORef z
