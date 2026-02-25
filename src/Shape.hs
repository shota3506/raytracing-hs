module Shape where

import Intersection
import Interval
import Material
import Ray

-- | A hittable shape in the scene.
-- Wraps a hit function that tests ray intersection
-- within a given t range.
newtype Shape = Shape
  { hit :: Ray -> Interval -> Maybe (Intersection, Material)
  }
