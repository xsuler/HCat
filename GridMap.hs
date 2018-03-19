module GridMap where

import           Algorithm
import           Data.Hashable     ()
import qualified Data.HashMap.Lazy as H
import           Data.Ix
import           Data.Matrix
import qualified Data.Text         as T

data Tag
  = Ground
  | Stone
  deriving (Eq, Show)

type GridMap = Matrix Tag

type Point = (Int, Int)

rangeM::GridMap->(Int,Int)
rangeM mp = (nrows mp,ncols mp)

-- |read the GridMap from raw text
readMap ::T.Text-> IO GridMap
readMap txt= do
  let ls=T.lines txt
  let fix = T.unpack $ T.concat ls
  return $
    fromList (length ls) (T.length $head ls) $
    map
      (\x ->
         if x == '1'
           then Stone
           else Ground)
      fix

-- |stategy to expand states
peek :: GridMap -> StateGen Point Int
peek mp pt =
  H.fromList $flip zip (repeat 1) $
  filter
    (\x -> inRange ((1, 1), rangeM mp) x && (uncurry unsafeGet x mp == Ground)) $
  bounds pt

bounds :: Point -> [Point]
bounds (a, b) = [(a + 1, b), (a - 1, b), (a, b - 1), (a, b + 1)]

mhdDis::Point->Point->Int
mhdDis (a,b) (c,d) = abs (a-c)+abs (b-d)

-- |based on dijkstra
shortestPath :: GridMap -> PathFinder Point
shortestPath mp s t
  |s==t = []
  |null ans&&mhdDis s t>1 = []
  |otherwise = t:ans
  where
    resTbl = shortestPathTable t s $ peek mp
    ans=getStatesR resTbl t
