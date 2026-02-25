module Dielectric where

import Intersection
import Material
import Ray
import System.Random (uniformR)
import Vec3 qualified as V

type Dielectric = Material

mkDielectric :: Double -> Dielectric
mkDielectric refIdx =
  Material {scatter = scatterDielectric}
  where
    -- Schlick's approximation for reflectance
    reflectance cosine refIndex =
      let r0 = (1 - refIndex) / (1 + refIndex)
          r0Squared = r0 * r0
       in r0Squared + (1 - r0Squared) * ((1 - cosine) ** 5)
    scatterDielectric ray isec gen =
      let attenuation = V.Vec3 1 1 1
          etaiOverEtat = if frontFace isec then 1.0 / refIdx else refIdx
          unitDirection = V.unit (direction ray)

          cosTheta = min (V.dot (V.negate unitDirection) (normal isec)) 1.0
          sinTheta = sqrt (max (1.0 - cosTheta * cosTheta) 0.0)
          (r, gen') = uniformR (0.0, 1.0) gen
          dir =
            if etaiOverEtat * sinTheta > 1.0 || reflectance cosTheta etaiOverEtat > r
              then V.reflect unitDirection (normal isec)
              else V.refract unitDirection (normal isec) etaiOverEtat
       in Just (attenuation, Ray {origin = point isec, direction = dir}, gen')
