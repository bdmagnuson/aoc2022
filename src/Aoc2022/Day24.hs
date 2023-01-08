module Aoc2022.Day24
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens hiding (Empty)
import Data.Attoparsec.Text qualified as P
import Data.Heap qualified as H
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace

data Wind = N | E | W | S deriving (Eq, Ord)

data Spot = Wall | Winds (Set Wind) deriving (Eq)

type Pos = (Int, Int)

newtype Board = Board {unBoard :: Map Pos Spot} deriving (Eq)

makePrisms ''Board

instance Show Wind where
  show N = "^"
  show S = "v"
  show E = ">"
  show W = "<"

instance Show Spot where
  show s =
    case s of
      Wall -> "#"
      Winds s ->
        case S.size s of
          0 -> "."
          1 -> show (head (S.elems s))
          n -> show n

instance Show Board where
  show (Board b) =
    let k = M.keys b
        showR :: Int -> String
        showR y = concatMap (showS y) [minX .. maxX]
        showS y x = show (b ^?! ix (y, x))
     in unlines (map showR [minY .. maxY])

parser = shape <$> P.sepBy pLine P.endOfLine
  where
    pLine =
      many
        ( (Winds (S.singleton E) <$ P.char '>')
            <|> (Winds (S.singleton W) <$ P.char '<')
            <|> (Winds (S.singleton N) <$ P.char '^')
            <|> (Winds (S.singleton S) <$ P.char 'v')
            <|> (Winds S.empty <$ P.char '.')
            <|> (Wall <$ P.char '#')
        )
    shape :: [[Spot]] -> Board
    shape m =
      let h = length m
          w = length (head m)
       in (Board . M.fromList) (zip [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1]] (concat m))

input = getInput "input/day24.txt" parser

locs = M.keys (unBoard input)

minX = minimum (locs ^.. folded . _2)

minY = minimum (locs ^.. folded . _1)

maxX = maximum (locs ^.. folded . _2)

maxY = maximum (locs ^.. folded . _1)

period = lcm (maxX - minX + 1 - 2) (maxY - minY + 1 - 2)

type MinHeap = H.MinPrioHeap Int (Int, Pos)

step :: Board -> Board
step (Board b) = Board (M.mapWithKey f b)
  where
    f _ Wall = Wall
    f p@(y, x) _
      | y == minY = Winds S.empty
      | y == maxY = Winds S.empty
      | otherwise = Winds $ S.fromList (catMaybes [fromWest p, fromEast p, fromNorth p, fromSouth p])
      where
        fromSouth (y, x) =
          let y' = if y + 1 == maxY then 1 else y + 1
              (Winds w) = b ^?! ix (y', x)
           in if N `S.member` w then Just N else Nothing
        fromNorth (y, x) =
          let y' = if y - 1 == minY then maxY - 1 else y - 1
              (Winds w) = b ^?! ix (y', x)
           in if S `S.member` w then Just S else Nothing
        fromEast (y, x) =
          let x' = if x - 1 == minX then maxX - 1 else x - 1
              (Winds w) = b ^?! ix (y, x')
           in if E `S.member` w then Just E else Nothing
        fromWest (y, x) =
          let x' = if x + 1 == maxX then minX + 1 else x + 1
              (Winds w) = b ^?! ix (y, x')
           in if W `S.member` w then Just W else Nothing

grids :: [Board]
grids = cycle $ take period (iterate step input)

startPt = (minY, 1)

endPt = (maxY, maxX - 1)

tr x = traceShow x x

solve rnd startPt end = go (H.singleton (0, start)) S.empty
  where
    start :: (Int, Pos)
    start = (rnd, startPt)
    go :: MinHeap -> Set (Int, Pos) -> Int
    go h v
      | H.size h == 0 = error "wtf"
      | S.member pair v = go (H.drop 1 h) v
      | pos == end = cost
      | otherwise = go h' v'
      where
        Just (cost, pair@(rnd, pos@(y, x))) = H.viewHead h
        v' = S.insert pair v
        h' = H.drop 1 h `H.union` newNodes
          where
            newNodes =
              let pts = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1), (y, x)]
                  pts' = filter (\(y, x) -> (p <$> ((grids !! (rnd + 1)) ^? _Board . ix (y, x))) == Just True) pts
                  p Wall = False
                  p (Winds s) = S.null s
               in H.fromList [(cost + 1, ((rnd + 1) `mod` period, p)) | p <- pts']

part1 = solve 0 startPt endPt

part2 =
  let cost1 = solve 0 startPt endPt
      cost2 = solve cost1 endPt startPt
      cost3 = solve (cost1 + cost2) startPt endPt
   in cost1 + cost2 + cost3
