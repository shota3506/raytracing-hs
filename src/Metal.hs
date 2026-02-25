module Metal where

import Color
import Intersection
import Material
import Ray
import Vec3 qualified as V

type Metal = Material

mkMetal :: Color -> Double -> Metal
mkMetal albedo fuzz =
  Material {scatter = scatterMetal}
  where
    scatterMetal ray isec gen =
      let (s, gen') = V.uniformSphere gen
          reflected = V.add (V.unit (V.reflect (direction ray) (normal isec))) (V.scale (min fuzz 1) s)
          scattered = Ray {origin = point isec, direction = reflected}
       in Just (albedo, scattered, gen')
