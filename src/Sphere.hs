module Sphere where

import Interval
import Ray
import Shape
import Vec3 (Point3)
import Vec3 qualified as V

data Sphere = Sphere {center :: Point3, radius :: Double}

toShape :: Sphere -> Shape
toShape (Sphere center radius) = Shape {hit = hitSphere}
  where
    r = max 0 radius
    hitSphere ray iv
      | discriminant < 0 = Nothing
      | surrounds iv t1 = Just (mkIntersection t1)
      | surrounds iv t2 = Just (mkIntersection t2)
      | otherwise = Nothing
      where
        oc = V.sub center (rayOrigin ray)
        a = V.dot (rayDirection ray) (rayDirection ray)
        h = V.dot oc (rayDirection ray)
        c = V.dot oc oc - r * r
        discriminant = h * h - a * c

        sqrtd = sqrt discriminant
        t1 = (h - sqrtd) / a
        t2 = (h + sqrtd) / a
        mkIntersection t' =
          let p = rayAt ray t'
              outwardNormal = V.div r (V.sub p center)
              (n, ff) = setFaceNormal ray outwardNormal
           in Intersection {point = p, normal = n, t = t', frontFace = ff}
