module Material where

import Color
import Intersection
import Ray
import System.Random (StdGen)

newtype Material = Material
  { scatter :: Ray -> Intersection -> StdGen -> Maybe (Color, Ray, StdGen)
  }
