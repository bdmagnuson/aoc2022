module Aoc2022.Day18
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace

data Face = F1 | F2 | F3 | F1O | F2O | F3O deriving (Eq, Ord, Show)

type Pt = (Integer, Integer, Integer)

type Surface = Set (Pt, Face)

input = getInput "input/day18.txt" parser

parser = P.sepBy1 (toTuple <$> (P.sepBy1 P.decimal (P.char ','))) P.endOfLine
  where
    toTuple (a : b : c : _) = (a, b, c)

addPt :: Set Surface -> Pt -> Set Surface
addPt s p = merge s (mkPoint p)

mkPoint p = S.fromList (zip (repeat p) [F1, F2, F3, F1O, F2O, F3O])

merge :: Set Surface -> Surface -> Set Surface
merge s p =
  let opp = S.map pair p
      (s1, s2) = S.partition (S.null . S.intersection opp) s
   in if S.null s2
        then S.insert p s1
        else S.insert (merge1 p (foldl1 S.union s2)) s1

merge1 :: Surface -> Surface -> Surface
merge1 p s =
  let opp = S.map pair p
      matching = S.intersection s opp
      ropp = S.map pair matching
   in S.union (S.difference p ropp) (S.difference s matching)

pair :: (Pt, Face) -> (Pt, Face)
pair ((x, y, z), F1) = ((x, y - 1, z), F1O)
pair ((x, y, z), F2) = ((x, y, z + 1), F2O)
pair ((x, y, z), F3) = ((x + 1, y, z), F3O)
pair ((x, y, z), F1O) = ((x, y + 1, z), F1)
pair ((x, y, z), F2O) = ((x, y, z - 1), F2)
pair ((x, y, z), F3O) = ((x - 1, y, z), F3)

faces = foldl1 S.union (foldl addPt S.empty input)

-- make list of points that steam can reach
steam =
  let p = (0, 0, 0)
      start = S.singleton p
   in go start (neighbors (0, 0, 0))
  where
    go :: Set Pt -> [Pt] -> Set Pt
    go v n
      | null n = v
      | S.member (head n) v = go v (tail n)
      | otherwise = go (S.insert (head n) v) (tail n ++ neighbors (head n))
    neighbors :: Pt -> [Pt]
    neighbors (x, y, z) =
      filter
        inRange
        [ (x + 1, y, z),
          (x - 1, y, z),
          (x, y + 1, z),
          (x, y - 1, z),
          (x, y, z + 1),
          (x, y, z - 1)
        ]
    inRange p@(x, y, z) = x >= -1 && x <= 21 && y >= -1 && y <= 21 && z >= -1 && z <= 21 && (not (S.member p inputSet))
    inputSet = S.fromList input

part1 = S.size faces

part2 = sum (map nFaces (S.toList steam))
  where
    -- Number of exposes faces a steam point touches
    nFaces :: Pt -> Int
    nFaces p = S.size (S.filter (\(p', _) -> p' == p) adjFaces)
    adjFaces = S.map pair faces
