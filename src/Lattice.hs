module Lattice
  (Lattice(..)
  )
where

class Lattice l where
  lub, glb  :: l -> l -> l
  bot, top  :: l
  canFlowTo :: l -> l -> Bool
