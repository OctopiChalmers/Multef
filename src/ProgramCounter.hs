-- | This module implements program counters
module ProgramCounter
  ( PC
  , emptyPC
  , emptyView
  , extendPositive
  , extendNegative
  , pcF
  )
where

import Lattice.Class
import Faceted

data Branch l = Positive l
              | Negative l
              deriving (Ord, Eq, Show)

newtype PC l = PC { branches :: [Branch l] }

-- | The empty PC
emptyPC :: PC l
emptyPC = PC []

-- | Extend the PC with a positive branch
extendPositive :: PC l -> l -> PC l
extendPositive (PC pc) l = PC (Positive l : pc)

-- | Extend the PC with a negative branch
extendNegative :: PC l -> l -> PC l
extendNegative (PC pc) l = PC (Negative l : pc)

-- | Compute the candidate label
candidate :: Lattice l => PC l -> l
candidate (PC pc) = foldr lub bot [ l | Positive l <- pc ]

-- | Check if the view associated to a PC is empty
emptyView :: Lattice l => PC l -> Bool
emptyView pc@(PC brs) = not $ all (\l -> not $ canFlowTo l (candidate pc)) [ l | Negative l <- brs ]

-- | Create a faceted value which behaves as `priv` to an observer
-- in the view of the PC and as `pub` to others.
pcF :: PC l -> Faceted l a -> Faceted l a -> Faceted l a
pcF (PC [])                      priv _   = priv
pcF (PC (Positive k : branches)) priv pub = Facet k (pcF (PC branches) priv pub) pub
pcF (PC (Negative k : branches)) priv pub = Facet k pub (pcF (PC branches) priv pub)
