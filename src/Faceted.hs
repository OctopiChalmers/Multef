{-# LANGUAGE GADTs #-}
module Faceted
  ( Faceted(..)
  , facet
  )
where
import Control.Monad

data Faceted l a where
  Raw   :: a -> Faceted l a
  Facet :: l -> Faceted l a -> Faceted l a -> Faceted l a

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
