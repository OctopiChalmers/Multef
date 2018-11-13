module Lattice where

class Lattice l where
  lub, glb  :: l -> l -> l
  bot, top  :: l
  canFlowTo :: l -> l -> Bool

instance (Lattice l, Lattice l') => Lattice (l, l') where
  (l0, l0') `lub` (l1, l1') = (l0 `lub` l1, l0' `lub` l1')
  (l0, l0') `glb` (l1, l1') = (l0 `glb` l1, l0' `glb` l1')
  bot = (bot, bot)
  top = (top, top)
  (l0, l0') `canFlowTo` (l1, l1') = l0 `canFlowTo` l1 && l0' `canFlowTo` l1'

newtype Dual l = Dual l

instance Lattice l => Lattice (Dual l) where
  (Dual l0) `lub` (Dual l1) = Dual (l0 `glb` l1)
  (Dual l0) `glb` (Dual l1) = Dual (l0 `lub` l1)
  bot = Dual top
  top = Dual bot
  (Dual l0) `canFlowTo` (Dual l1) = l1 `canFlowTo` l0
