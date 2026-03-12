module BVH where

import AABB
import AABB qualified as A
import Data.List (sortBy)
import Data.Ord (comparing)
import Intersection
import Interval
import Shape

buildBVH :: [Shape] -> Shape
buildBVH [] = Shape {hit = \_ _ -> Nothing, boundingBox = A.empty}
buildBVH [s] = s
buildBVH shapes = Shape {hit = hitBVH, boundingBox = bb}
  where
    axis = longestAxis (foldl1 A.enclose (map boundingBox shapes))
    sorted = sortBy (comparing (imin . axis . boundingBox)) shapes
    (ls, rs) = splitAt (div (length sorted) 2) sorted
    l = buildBVH ls
    r = buildBVH rs
    bb = A.enclose (boundingBox l) (boundingBox r)
    hitBVH ray iv
      | not (intersects bb ray iv) = Nothing
      | otherwise =
          let leftHit = hit l ray iv
              iv' = case leftHit of
                Just (isec, _) -> Interval (imin iv) (t isec)
                Nothing -> iv
              hitRight = hit r ray iv'
           in case hitRight of
                Just _ -> hitRight
                Nothing -> leftHit

longestAxis :: AABB -> (AABB -> Interval)
longestAxis (AABB x y z) =
  let xl = imax x - imin x
      yl = imax y - imin y
      zl = imax z - imin z
   in if xl >= yl && xl >= zl
        then A.x
        else if yl >= zl then A.y else A.z
