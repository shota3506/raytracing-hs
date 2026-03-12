module Interval where

data Interval = Interval
  { imin :: Double,
    imax :: Double
  }
  deriving (Show, Eq)

enclose :: Interval -> Interval -> Interval
enclose (Interval imin1 imax1) (Interval imin2 imax2) =
  Interval (min imin1 imin2) (max imax1 imax2)

contains :: Interval -> Double -> Bool
contains (Interval imin imax) x = x >= imin && x <= imax

surrounds :: Interval -> Double -> Bool
surrounds (Interval imin imax) x = x > imin && x < imax

clamp :: Interval -> Double -> Double
clamp (Interval imin imax) x = max imin (min imax x)

expand :: Interval -> Double -> Interval
expand (Interval imin imax) delta =
  Interval (imin - padding) (imax + padding)
  where
    padding = delta / 2

empty :: Interval
empty = Interval (1 / 0) (-(1 / 0))

universe :: Interval
universe = Interval (-(1 / 0)) (1 / 0)
