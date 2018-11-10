module ProgramCounter
  ( PC
  , emptyView
  , extendPositive
  , extendNegative
  , pcF
  )
where

import Lattice
import Faceted

data Branch l = Positive l
              | Negative l
              deriving (Ord, Eq, Show)

newtype PC l = PC { branches :: [Branch l] }

extendPositive :: PC l -> l -> PC l
extendPositive (PC pc) l = PC (Positive l : pc)

extendNegative :: PC l -> l -> PC l
extendNegative (PC pc) l = PC (Negative l : pc)

candidate :: Lattice l => PC l -> l
candidate (PC pc) = foldr lub bot [ l | Positive l <- pc ]

emptyView :: Lattice l => PC l -> Bool
emptyView pc@(PC brs) = all (\l -> not $ canFlowTo l (candidate pc)) [ l | Negative l <- brs ]

pcF :: PC l -> Faceted l a -> Faceted l a -> Faceted l a
pcF (PC [])                      priv _   = priv
pcF (PC (Positive k : branches)) priv pub = Facet k (pcF (PC branches) priv pub) pub
pcF (PC (Negative k : branches)) priv pub = Facet k pub (pcF (PC branches) priv pub)
