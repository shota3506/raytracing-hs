module Shape where

import Interval
import Ray
import Vec3 (Point3, Vec3)
import Vec3 qualified as V

data Intersection = Intersection
  { point :: Point3,
    normal :: Vec3,
    t :: Double,
    frontFace :: Bool
  }
  deriving (Show, Eq)

setFaceNormal :: Ray -> Vec3 -> (Vec3, Bool)
setFaceNormal r outwardNormal =
  if V.dot (rayDirection r) outwardNormal < 0
    then (outwardNormal, True)
    else (V.negate outwardNormal, False)

-- | A hittable shape in the scene.
-- Wraps a hit function that tests ray intersection
-- within a given t range.
newtype Shape = Shape
  { hit :: Ray -> Interval -> Maybe Intersection
  }
