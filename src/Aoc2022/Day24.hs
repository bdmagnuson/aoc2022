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

data Board = Board {unBoard :: (Map Pos Spot)} deriving (Eq)

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
        showR y = concatMap (showS y) [min_x .. max_x]
        showS y x = show (b ^?! ix (y, x))
     in unlines (map showR [min_y .. max_y])

parser = shape <$> P.sepBy pLine P.endOfLine
  where
    pLine =
      many
        ( ( (Winds (S.singleton E) <$ P.char '>')
              <|> (Winds (S.singleton W) <$ P.char '<')
              <|> (Winds (S.singleton N) <$ P.char '^')
              <|> (Winds (S.singleton S) <$ P.char 'v')
              <|> (Winds (S.empty) <$ P.char '.')
              <|> (Wall <$ P.char '#')
          )
        )
    shape :: [[Spot]] -> Board
    shape m =
      let h = length m
          w = length (head m)
       in (Board . M.fromList) (zip [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1]] (concat m))

input = getInput "input/day24.txt" parser

locs = M.keys (unBoard input)

min_x = minimum (locs ^.. folded . _2)

min_y = minimum (locs ^.. folded . _1)

max_x = maximum (locs ^.. folded . _2)

max_y = maximum (locs ^.. folded . _1)

period = lcm (max_x - min_x + 1 - 2) (max_y - min_y + 1 - 2)

type MinHeap = H.MinPrioHeap Int (Int, Pos)

step :: Board -> Board
step (Board b) = Board (M.mapWithKey f b)
  where
    f _ Wall = Wall
    f p@(y, x) _
      | y == min_y = Winds S.empty
      | y == max_y = Winds S.empty
      | otherwise = Winds $ S.fromList (catMaybes [fromWest p, fromEast p, fromNorth p, fromSouth p])
      where
        fromSouth (y, x) =
          let y' = if y + 1 == max_y then 1 else y + 1
              (Winds w) = b ^?! ix (y', x)
           in if N `S.member` w then (Just N) else Nothing
        fromNorth (y, x) =
          let y' = if y - 1 == min_y then max_y - 1 else y - 1
              (Winds w) = b ^?! ix (y', x)
           in if S `S.member` w then (Just S) else Nothing
        fromEast (y, x) =
          let x' = if x - 1 == min_x then max_x - 1 else x - 1
              (Winds w) = b ^?! ix (y, x')
           in if E `S.member` w then (Just E) else Nothing
        fromWest (y, x) =
          let x' = if x + 1 == max_x then min_x + 1 else x + 1
              (Winds w) = b ^?! ix (y, x')
           in if W `S.member` w then (Just W) else Nothing

grids :: [Board]
grids = cycle $ take period (iterate step input)

start_pt = (min_y, 1)

end_pt = (max_y, max_x - 1)

tr x = traceShow x x

solve rnd start_pt end = go (H.singleton (0, start)) (S.empty)
  where
    start :: (Int, Pos)
    start = (rnd, start_pt)
    go :: MinHeap -> Set (Int, Pos) -> Int
    go h v
      | H.size h == 0 = error "wtf"
      | S.member pair v = go (H.drop 1 h) v
      | pos == end = cost
      | otherwise = go h' v'
      where
        Just (cost, pair@(rnd, pos@(y, x))) = H.viewHead h
        v' = S.insert pair v
        h' = (H.drop 1 h) `H.union` newNodes
          where
            newNodes =
              let pts = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1), (y, x)]
                  pts' = filter (\(y, x) -> (p <$> ((grids !! (rnd + 1)) ^? _Board . ix (y, x))) == Just True) pts
                  p Wall = False
                  p (Winds s) = S.null s
               in H.fromList [(cost + 1, ((rnd + 1) `mod` period, p)) | p <- pts']

part1 = solve 0 start_pt end_pt

part2 =
  let cost1 = solve 0 start_pt end_pt
      cost2 = solve cost1 end_pt start_pt
      cost3 = solve (cost1 + cost2) start_pt end_pt
   in cost1 + cost2 + cost3
