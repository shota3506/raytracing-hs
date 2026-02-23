module Interval where

data Interval = Interval
  { imin :: Double,
    imax :: Double
  }
  deriving (Show, Eq)

contains :: Interval -> Double -> Bool
contains (Interval imin imax) x = x >= imin && x <= imax

surrounds :: Interval -> Double -> Bool
surrounds (Interval imin imax) x = x > imin && x < imax

clamp :: Interval -> Double -> Double
clamp (Interval imin imax) x = max imin (min imax x)

empty :: Interval
empty = Interval (1 / 0) (-(1 / 0))

universe :: Interval
universe = Interval (-(1 / 0)) (1 / 0)
