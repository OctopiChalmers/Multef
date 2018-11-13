module Lattice.CNF where

import qualified Data.Set as S

import Lattice.Class

data CNF a = CNF (S.Set (S.Set a))

instance Ord a => Lattice (CNF a) where
  CNF l0 `lub` CNF l1 = CNF (l0 `S.union` l1)

  CNF l0 `glb` CNF l1 = CNF (uncurry S.union `S.map` S.cartesianProduct l0 l1)

  top = CNF (S.singleton S.empty)
  bot = CNF S.empty

  CNF l0 `canFlowTo` CNF l1 = all (\c -> any (\c' -> c' `S.isSubsetOf` c) (S.toList l1)) (S.toList l1)

singleton :: Ord a => a -> CNF a
singleton a = CNF (S.singleton (S.singleton a))
