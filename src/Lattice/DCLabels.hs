module Lattice.DCLabels where

import Lattice.Class
import Lattice.CNF

-- | A DC-label is a CNF and a dual of a CNF
newtype DCLabels a = DC (CNF a, Dual (CNF a))

instance Principal DCLabels where
  singleton a = DC (singleton a , Dual (singleton a))

instance Ord a => Lattice (DCLabels a) where
  DC l0 `lub` DC l1 = DC $ l0 `lub` l1
  DC l0 `glb` DC l1 = DC $ l0 `glb` l1
  top = DC top
  bot = DC bot
  DC l0 `canFlowTo` DC l1 = l0 `canFlowTo` l1
