module Lattice.Powerset where

import qualified Data.Set as S

import Lattice.Class

data Powerset a = Top
                | Power (S.Set a)
                deriving (Eq, Show)

instance Principal Powerset where
  singleton = Power . S.singleton

instance Ord a => Lattice (Powerset a) where
  Top      `lub` _        = Top
  _        `lub` Top      = Top
  Power s0 `lub` Power s1 = Power (s0 `S.union` s1)

  Top      `glb` a        = a
  a        `glb` Top      = a
  Power s0 `glb` Power s1 = Power (s0 `S.intersection` s1)

  top = Top
  bot = Power S.empty

  _        `canFlowTo` Top      = True
  Top      `canFlowTo` _        = False
  Power s0 `canFlowTo` Power s1 = s0 `S.isSubsetOf` s1
