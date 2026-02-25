module Ray where

import Vec3 (Point3, Vec3)
import Vec3 qualified as V

data Ray = Ray
  { origin :: Point3,
    direction :: Vec3
  }

rayAt :: Ray -> Double -> Point3
rayAt (Ray orig dir) t = V.add orig (V.scale t dir)
