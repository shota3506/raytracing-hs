module Sphere where

import Ray
import Shape
import Vec3

data Sphere = Sphere {center :: Point3, radius :: Double}

toShape :: Sphere -> Shape
toShape (Sphere center radius) = Shape {hit = hitSphere}
  where
    r = max 0 radius
    hitSphere ray tmin tmax
      | discriminant < 0 = Nothing
      | t1 > tmin && t1 < tmax = Just (mkIntersection t1)
      | t2 > tmin && t2 < tmax = Just (mkIntersection t2)
      | otherwise = Nothing
      where
        oc = center `vSub` rayOrigin ray
        a = dot (rayDirection ray) (rayDirection ray)
        h = dot oc (rayDirection ray)
        c = dot oc oc - r * r
        discriminant = h * h - a * c

        sqrtd = sqrt discriminant
        t1 = (h - sqrtd) / a
        t2 = (h + sqrtd) / a
        mkIntersection t' =
          let p = rayAt ray t'
              outwardNormal = vDiv r (p `vSub` center)
              (n, ff) = setFaceNormal ray outwardNormal
           in Intersection {point = p, normal = n, t = t', frontFace = ff}
