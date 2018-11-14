module Syntax where

import Control.Monad

import FIO

when' :: Lattice l => Faceted l Bool -> FIO l a -> FIO l ()
when' fac comp = do
  control $ do
    cond <- fac
    return $ if cond
             then void comp
             else return ()
  return ()
