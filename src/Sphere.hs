module Sphere where

import Intersection
import Interval
import Material
import Ray
import Ray qualified as R
import Shape
import Vec3 (Point3, Vec3)
import Vec3 qualified as V

data Sphere = Sphere {center :: Point3, movement :: Vec3, radius :: Double, material :: Material}

toShape :: Sphere -> Shape
toShape (Sphere center movement radius material) = Shape {hit = hitSphere}
  where
    r = max 0 radius
    hitSphere ray iv
      | discriminant < 0 = Nothing
      | surrounds iv t1 = Just (mkIntersection t1)
      | surrounds iv t2 = Just (mkIntersection t2)
      | otherwise = Nothing
      where
        currentCenter = V.add center (V.scale (R.time ray) movement)
        oc = V.sub currentCenter (origin ray)
        a = V.dot (R.direction ray) (R.direction ray)
        h = V.dot oc (R.direction ray)
        c = V.dot oc oc - r * r
        discriminant = h * h - a * c

        sqrtd = sqrt discriminant
        t1 = (h - sqrtd) / a
        t2 = (h + sqrtd) / a
        mkIntersection t' =
          let p = rayAt ray t'
              outwardNormal = V.div r (V.sub p currentCenter)
              (n, ff) = setFaceNormal ray outwardNormal
           in (Intersection {point = p, normal = n, t = t', frontFace = ff}, material)
