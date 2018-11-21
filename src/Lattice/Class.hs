{-# LANGUAGE FlexibleInstances #-}
module Lattice.Class where

import GHC.Exts

-- | For lattices which are built on top of
-- the notion of an atomic principal
class Principal l where
  singleton :: a -> l a

-- | String principals made handy
instance Principal l => IsString (l String) where
  fromString = singleton

class Lattice l where
  lub, glb  :: l -> l -> l
  bot, top  :: l
  canFlowTo :: l -> l -> Bool

-- | The product lattice
instance (Lattice l, Lattice l') => Lattice (l, l') where
  (l0, l0') `lub` (l1, l1') = (l0 `lub` l1, l0' `lub` l1')
  (l0, l0') `glb` (l1, l1') = (l0 `glb` l1, l0' `glb` l1')
  bot = (bot, bot)
  top = (top, top)
  (l0, l0') `canFlowTo` (l1, l1') = l0 `canFlowTo` l1 && l0' `canFlowTo` l1'

-- | Constructs the dual of a lattice
newtype Dual l = Dual l deriving Eq

instance Lattice l => Lattice (Dual l) where
  (Dual l0) `lub` (Dual l1) = Dual (l0 `glb` l1)
  (Dual l0) `glb` (Dual l1) = Dual (l0 `lub` l1)
  bot = Dual top
  top = Dual bot
  (Dual l0) `canFlowTo` (Dual l1) = l1 `canFlowTo` l0
