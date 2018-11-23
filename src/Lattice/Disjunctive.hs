module Lattice.Disjunctive (Disjunctive) where

import Lattice.Class

import Data.Set as S

data Disjunctive p = Bottom
                   | Top
                   | Disjunction (Set p)
                   deriving (Ord, Eq, Show)

instance Principal Disjunctive where
  singleton p = Disjunction (S.singleton p)

instance (Ord p, Eq p) => Lattice (Disjunctive p) where
  Bottom `lub` a = a
  a `lub` Bottom = a
  _ `lub` Top    = Top
  Top `lub` _    = Top
  Disjunction s0 `lub` Disjunction s1 = if s0 == s1 then Disjunction s0 else Top

  Bottom `glb` _ = Bottom
  _ `glb` Bottom = Bottom
  a `glb` Top    = a
  Top `glb` a    = a
  Disjunction s0 `glb` Disjunction s1 = Disjunction $ s0 `union` s1

  bot = Bottom
  top = Top

  Bottom `canFlowTo` _ = True
  _ `canFlowTo` Top    = True
  Disjunction  s0 `canFlowTo` Disjunction s1 = s1 `isSubsetOf` s0
  _ `canFlowTo` _ = False
