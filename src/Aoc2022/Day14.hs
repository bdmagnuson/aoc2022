module Aoc2022.Day14
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens hiding (Empty)
import Data.Attoparsec.Text qualified as P
import Data.List (foldl', unfoldr)
import Data.Map (Map)
import Data.Map qualified as M
import Debug.Trace

data Elem = Sand | Empty | Rock deriving (Show, Eq)

type Pt = (Integer, Integer)

type St = Map Pt Elem

data Part = Part1 | Part2 deriving (Show)

parser = P.sepBy1 (P.sepBy1 pPair (P.string " -> ")) P.endOfLine
  where
    pPair = do
      l <- P.decimal
      P.char ','
      r <- P.decimal
      return (l, r)

input = getInput "input/day14.txt" parser :: [[Pt]]

allPts = (500, 0) : concat input

min_x = minimum (allPts ^.. folded . _1)

min_y = minimum (allPts ^.. folded . _2)

max_x = maximum (allPts ^.. folded . _1)

max_y = maximum (allPts ^.. folded . _2)

initMap = foldl' insertRocks emptyMap input
  where
    fillMap ((x1, y1), (x2, y2)) e =
      let (x1', x2') = if x1 > x2 then (x2, x1) else (x1, x2)
          (y1', y2') = if y1 > y2 then (y2, y1) else (y1, y2)
       in M.fromList (zip [(x, y) | y <- [y1' .. y2'], x <- [x1' .. x2']] (repeat e))
    emptyMap = fillMap ((min_x, min_y), (max_x, max_y)) Empty
    insertRocks m s = foldl' insertRockSegment m (zip s (tail s))
    insertRockSegment m ps = M.union (fillMap ps Rock) m

dropSand :: Part -> St -> Maybe Pt
dropSand part s = go (500, 0)
  where
    go p@(x, y) =
      let down = (x, y + 1)
          left = (x - 1, y + 1)
          right = (x + 1, y + 1)
          move :: Pt -> Maybe Elem
          move p@(x, y) =
            case s ^? ix p of
              Just a -> Just a
              Nothing ->
                case part of
                  Part1 -> Nothing
                  Part2 ->
                    if ((y >= 0) && (y <= max_y + 1))
                      then Just Empty
                      else Just Rock
       in case (move down, move left, move right) of
            (Nothing, _, _) -> Nothing
            (Just Empty, _, _) -> go down
            (_, Nothing, _) -> Nothing
            (_, Just Empty, _) -> go left
            (_, _, Nothing) -> Nothing
            (_, _, Just Empty) -> go right
            otherwise -> Just p

res :: Part -> [Pt]
res p = unfoldr f initMap
  where
    f s = case dropSand p s of
      Nothing -> Nothing
      Just p -> Just (p, s & at p .~ Just Sand)

part1 = length (res Part1)

part2 = 1 + (length $ (takeWhile (/= (500, 0))) (res Part2))
