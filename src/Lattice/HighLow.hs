module Lattice.HighLow where

import Lattice

data HighLow = H | L deriving (Ord, Eq, Show)

instance Lattice HighLow where
  lub H _ = H
  lub _ H = H
  lub _ _ = L

  glb L _ = L
  glb _ L = L
  glb _ _ = H

  bot = L
  top = H

  canFlowTo H L = False
  canFlowTo _ _ = True
