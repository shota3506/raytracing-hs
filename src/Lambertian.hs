module Lambertian where

import Color
import Intersection
import Material
import Ray
import Vec3 qualified as V

type Lambertian = Material

mkLambertian :: Color -> Lambertian
mkLambertian albedo =
  Material {scatter = scatterLambertian}
  where
    scatterLambertian ray isec gen =
      let (v, gen1) = V.uniformSphere gen
          scatterDir =
            let d = V.add (normal isec) v
             in if V.isNearZero d then normal isec else d
          scattered = Ray {origin = point isec, direction = scatterDir, time = time ray}
       in Just (albedo, scattered, gen1)
