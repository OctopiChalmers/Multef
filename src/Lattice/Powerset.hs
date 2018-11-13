module Lattice.Powerset where

import qualified Data.Set as S

import Lattice.Class

-- | The Powerset lattice. Because we don't have a notion
-- of a universe which we can represent we need an explicit
-- representation of `Top`
data Powerset a = Top
                | Power (S.Set a)
                deriving (Eq, Show)

-- | Elements of the powerset lattice are typically formed
-- by taking single principals and sticking them together
-- using `lub`
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
