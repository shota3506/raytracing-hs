module Intersection where

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
  if V.dot (direction r) outwardNormal < 0
    then (outwardNormal, True)
    else (V.negate outwardNormal, False)
