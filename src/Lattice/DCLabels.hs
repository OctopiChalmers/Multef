module Lattice.DCLabels where

import Lattice.Class
import Lattice.CNF

type DCLabels a = (CNF a, Dual (CNF a))
