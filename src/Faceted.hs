module Faceted
  ( Faceted(..)
  , facet
  )
where
import Control.Monad

-- | Representation of Faceted Values
data Faceted l a = Raw a
                 | Facet l (Faceted l a) (Faceted l a)
                 | Bot

-- | Create a Faceted Value
facet :: l -> Faceted l a -> Faceted l a -> Faceted l a
facet = Facet

instance Monad (Faceted l) where
  return = Raw

  Raw a         >>= k = k a
  Facet l fl fr >>= k = Facet l (fl >>= k) (fr >>= k)

instance Applicative (Faceted l) where
  pure  = return
  (<*>) = ap

instance Functor (Faceted l) where
  fmap = liftM
