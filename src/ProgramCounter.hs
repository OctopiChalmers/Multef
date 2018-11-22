-- | This module implements program counters
module ProgramCounter
  ( PC (..)
  , Branch (..)
  , emptyPC
  , emptyView
  , extendPositive
  , extendNegative
  , pcF
  , inViews
  )
where

import Lattice.Class
import Faceted

data Branch l = Positive l
              | Negative l
              deriving (Ord, Eq, Show)

newtype PC l = PC { branches :: [Branch l] }

instance Show l => Show (PC l) where
  show (PC bs) = show bs

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
candidate pc = foldr lub bot [ l | Positive l <- branches pc ]

-- | The positive branches
positives :: PC l -> [l]
positives pc = [ l | Positive l <- branches pc ]

-- | The negative branches
negatives :: PC l -> [l]
negatives pc = [ l | Negative l <- branches pc ]

-- | Check if a label is in the view of a pc
inViews :: Lattice l => l -> PC l -> Bool
inViews l pc =  all (`canFlowTo` l) (positives pc)
             && all (not . (`canFlowTo` l)) (negatives pc)

-- | Check if the view associated to a PC is empty
emptyView :: Lattice l => PC l -> Bool
emptyView pc = candidate pc `inViews` pc

-- | Create a faceted value which behaves as `priv` to an observer
-- in the view of the PC and as `pub` to others.
pcF :: PC l -> Faceted l a -> Faceted l a -> Faceted l a
pcF (PC [])                      priv _   = priv
pcF (PC (Positive k : branches)) priv pub = Facet k (pcF (PC branches) priv pub) pub
pcF (PC (Negative k : branches)) priv pub = Facet k pub (pcF (PC branches) priv pub)
