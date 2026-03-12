module Shape where

import AABB
import Intersection
import Interval
import Material
import Ray

-- | A hittable shape in the scene.
-- Wraps a hit function that tests ray intersection
-- within a given t range.
data Shape = Shape
  { hit :: Ray -> Interval -> Maybe (Intersection, Material),
    boundingBox :: AABB
  }
