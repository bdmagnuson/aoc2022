module Aoc2022.Day16
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Function.Memoize
import Data.Heap qualified as H
import Data.List (maximumBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

type Node = String

(p, g) = (getInput "input/day16.txt" parser)

parser = do
  (l, f, n) <- unzip3 <$> (P.sepBy1 pLine P.endOfLine)
  return (M.fromList (zip l f), M.fromList (zip l n))
  where
    pLine :: P.Parser (Node, Int, [Node])
    pLine = do
      P.string "Valve "
      l <- pLabel
      P.string " has flow rate="
      f <- P.decimal
      P.string "; tunnels lead to valves " <|> P.string "; tunnel leads to valve "
      n <- (P.sepBy pLabel (P.string ", "))
      return $ (l, f, n)
    pLabel = T.unpack <$> P.takeWhile (P.inClass ['A' .. 'Z'])

type MinHeap = H.MinPrioHeap Int Node

-- Number of hops from node f to t
nodeDistance = memoize2 nodeDistance'

nodeDistance' :: Node -> Node -> Int
nodeDistance' f t = go S.empty (newNode 1 f)
  where
    go :: Set Node -> MinHeap -> Int
    go v h
      | H.size h == 0 = error "wtf"
      | S.member n v = go v (H.drop 1 h)
      | n == t = d
      | otherwise = go v' h'
      where
        Just (d, n) = H.viewHead h
        v' = S.insert n v
        h' = (H.drop 1 h) `H.union` (newNode (d + 1) n)
    newNode :: Int -> Node -> MinHeap
    newNode d n = H.fromList (zip (repeat d) (g ^?! ix n))

-- Return best score with 'n' minutes to go visting 't' nodes
solve n t = go n (0, 0) "AA" t
  where
    go :: Int -> (Int, Int) -> Node -> S.Set Node -> Int
    go m (total, vent) n s
      | S.null s = total + vent * m
      | otherwise = maximum (map f (S.toList s))
      where
        f t =
          let dist = nodeDistance n t + 1
           in if dist >= m
                then (total + vent * m)
                else go (m - dist) (total + vent * dist, vent + p ^?! ix t) t (S.delete t s)

targets = S.fromList (M.keys (M.filter (> 0) p))

part1 = solve 30 targets

-- For part 2 calculate all scores for all subsets of the non-zero valves.  Then take the maximum of each
-- score paried with the score of it's complement.  Takes almost 2m but gets the right answer.
part2 = maximum (S.map (\s -> res ^?! ix s + res ^?! ix (S.difference targets s)) (S.powerSet targets))
  where
    res = M.fromSet (solve 26) (S.powerSet targets)
