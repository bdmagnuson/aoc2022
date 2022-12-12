module Aoc2022.Day12
  ( part1,
    part2,
  )
where

import Algebra.Graph.AdjacencyMap qualified as G
import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Char (isAlpha)
import Data.Heap qualified as H
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as S

data Point = Start | End | Height Char deriving (Show, Eq)

type Graph = G.AdjacencyMap (Int, Int)

type Coord = (Int, Int)

type Next = H.MinPrioHeap Int Coord

parser = many (many pPoint <* P.endOfLine)
  where
    pPoint = (Start <$ P.char 'S') <|> (End <$ P.char 'E') <|> (Height <$> P.satisfy isAlpha)

input = getInput "input/day12.txt" parser

isStep :: Char -> Char -> Bool
isStep a b = succ a == b || b <= a

toChar :: Point -> Char
toChar Start = 'a'
toChar End = 'z'
toChar (Height c) = c

start :: [[Point]] -> (M.Map Coord Char, Coord, Coord)
start ps = (pmap, st, end)
  where
    pmap = M.fromList (pts & traverse . _2 %~ toChar)
    width = length (head ps)
    height = length ps
    pts :: [(Coord, Point)]
    pts = zip [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1]] (concat ps)
    st = fst . fromJust . find (\(_, x) -> x == Start) $ pts
    end = fst . fromJust . find (\(_, x) -> x == End) $ pts

mkGraph :: M.Map Coord Char -> Graph
mkGraph m = foldl G.overlay G.empty (concatMap f (M.toList m))
  where
    f (pt, c1) =
      let v1 = G.vertex pt
          vs = map (G.vertex . fst) . filter p $ [(k, m ^? ix k) | k <- adj pt]
          p (_, Nothing) = False
          p (_, Just c2) = isStep c1 c2
       in map (G.connect v1) vs
    adj :: (Int, Int) -> [(Int, Int)]
    adj (y, x) =
      [ (y + 1, x),
        (y - 1, x),
        (y, x - 1),
        (y, x + 1)
      ]

path :: Coord -> Coord -> Graph -> Maybe Int
path start end g = go (newNodes 0 start) S.empty
  where
    go :: Next -> S.Set Coord -> Maybe Int
    go h s
      | H.size h == 0 = Nothing
      | dst == end = Just cost
      | dst `S.member` s = go (H.drop 1 h) s
      | otherwise = go h' s'
      where
        Just (cost, dst) = H.viewHead h
        h' = (H.drop 1 h) `H.union` (newNodes cost dst)
        s' = S.insert dst s
    adj = G.adjacencyMap g
    newNodes :: Int -> Coord -> Next
    newNodes cost dst = H.fromList . S.toList . S.map (\n -> (cost + 1, n)) $ (adj ^?! ix dst)

(m, st, end) = start input

g = mkGraph m

part1 = path st end g

part2 = minimum . catMaybes $ map (\x -> path x end g) startPoints
  where
    startPoints = M.keys (M.filter (== 'a') m)
