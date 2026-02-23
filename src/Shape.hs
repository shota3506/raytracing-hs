module Shape where

import Ray
import Vec3

data Intersection = Intersection
  { point :: Point3,
    normal :: Vec3,
    t :: Double,
    frontFace :: Bool
  }
  deriving (Show, Eq)

setFaceNormal :: Ray -> Vec3 -> (Vec3, Bool)
setFaceNormal r outwardNormal =
  if dot (rayDirection r) outwardNormal < 0
    then (outwardNormal, True)
    else (vNegate outwardNormal, False)

-- | A hittable shape in the scene.
-- Wraps a hit function that tests ray intersection
-- within a given t range.
newtype Shape = Shape
  { hit :: Ray -> Double -> Double -> Maybe Intersection
  }
