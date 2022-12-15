module Aoc2022.Day15
  ( part1,
    part2,
  )
where

import Advent.Box
import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.List (nub, sortBy)
import Data.Maybe (catMaybes)

type Coord = (Int, Int)

data Sensor = Sensor Coord Int deriving (Show)

parser = P.sepBy1 pPair P.endOfLine
  where
    loc = P.signed P.decimal
    pPair = do
      P.string "Sensor at x="
      sx <- loc
      P.string ", y="
      sy <- loc
      P.string ": closest beacon is at x="
      bx <- loc
      P.string ", y="
      by <- loc
      return $ (Sensor (sx, sy) (abs (sx - bx) + abs (sy - by)), (bx, by))

(diamonds, beacons) = unzip $ getInput "input/day15.txt" parser

noBeaconRow :: Int -> Sensor -> Maybe (Int, Int)
noBeaconRow row (Sensor (sx, sy) d) =
  let wing = d - (abs (sy - row))
   in if wing < 0 then Nothing else Just (sx - wing, sx + wing)

row = 2000000

part1 =
  let rngs = merge (sortBy g (catMaybes (map (noBeaconRow row) diamonds)))
      total = foldl (\acc (l, r) -> acc + (r - l + 1)) 0 rngs
      beaconRow = nub $ map fst $ filter (\(_, y) -> y == row) beacons
      sub = sum . (map (\x -> if x then 1 else 0)) $ map (\x -> any (\(l, r) -> x >= l || x <= r) rngs) beaconRow
   in total - sub
  where
    g (l1, r1) (l2, r2) =
      case compare l1 l2 of
        LT -> LT
        GT -> GT
        EQ -> compare r1 r2
    merge :: [(Int, Int)] -> [(Int, Int)]
    merge [] = []
    merge (x : []) = [x]
    merge ((l1, r1) : rest@((l2, r2) : xs)) = if l2 <= r1 + 1 then merge ((l1, max r1 r2) : xs) else (l1, r1) : (merge rest)

-- Make diamonds into rectangles with a change of basis.  Then we can subtract rectangles to get the final location and convert back to 'diamond' cooridinates.

cb (x, y) = (x + y, -x + y)

icb (x, y) = ((x - y) `div` 2, (x + y) `div` 2)

diamond2rect :: Sensor -> Box ('S ('S 'Z))
diamond2rect (Sensor (sx, sy) d) =
  let (x1, y1) = cb (sx - d, sy)
      (x2, y2) = cb (sx + d, sy)
   in Dim (min x1 x2) ((max x1 x2) + 1) (Dim (min y1 y2) ((max y1 y2) + 1) Pt)

rect2diamond :: Box ('S ('S 'Z)) -> ((Int, Int), (Int, Int))
rect2diamond (Dim x1 x2 (Dim y1 y2 Pt)) = (icb (x1, y1), icb (x2 - 1, y2 - 1))

subtractAllOf :: [Box n] -> Box n -> [Box n]
subtractAllOf xs ys = foldl remove1 [ys] xs
  where
    remove1 acc x = concatMap (subtractBox x) acc

search = 4000000

part2 =
  let start = diamond2rect (Sensor ((search `div` 2), (search `div` 2)) (search))
      left = map rect2diamond (subtractAllOf (map diamond2rect diamonds) start)
      -- Only one box should remains that doesn't have a corner outside of the search space so just filter out anthing outside the space.
      [(x, y)] = filter (\(x, y) -> x >= 0 && x <= search && y >= 0 && y <= search) (map fst left)
   in 4000000 * x + y
