module Aoc2022.Day23
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens hiding (Empty)
import Data.Attoparsec.Text qualified as P
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace

data Pos = Elf | Empty deriving (Show, Eq, Ord)

data Dir = N | W | E | S deriving (Show)

data Board = Board (Set (Int, Int)) deriving (Eq)

instance Show Board where
  show (Board b) =
    let k = S.elems b
        min_x = minimum (k ^.. folded . _2)
        min_y = minimum (k ^.. folded . _1)
        max_x = maximum (k ^.. folded . _2)
        max_y = maximum (k ^.. folded . _1)
        showR :: Int -> String
        showR y = map (\x -> if S.member (y, x) b then '#' else '.') [min_x .. max_x]
     in unlines (map showR [min_y .. max_y])

type Move = ((Int, Int), (Int, Int))

parser = shape <$> P.sepBy pLine P.endOfLine
  where
    pLine = many ((Elf <$ P.char '#') <|> (Empty <$ P.char '.'))
    shape :: [[Pos]] -> Board
    shape m =
      let h = length m
          w = length (head m)
       in Board . S.fromList . map fst . filter (\(_, b) -> b == Elf) $ (zip [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1]] (concat m))

input = getInput "input/day23.txt" parser

step :: [Dir] -> Board -> Board
step d (Board b) = Board (doMove . checkMoves $ proposeMoves)
  where
    doMove m =
      let (f, t) = unzip m
          b' = foldl (flip S.delete) b f
       in foldl (flip S.insert) b' t

    checkMoves :: [Move] -> [Move]
    checkMoves m =
      let counts :: Map (Int, Int) Int
          counts = foldl (\acc (_, p) -> acc & at p . non 0 %~ succ) M.empty m
          clashes = M.filter (== 1) counts
       in filter (((flip M.member) clashes) . snd) m

    proposeMoves :: [Move]
    proposeMoves =
      let elves = S.elems b
       in catMaybes $ map (proposeMove d) elves
    proposeMove :: [Dir] -> (Int, Int) -> Maybe Move
    proposeMove [] _ = Nothing
    proposeMove (d : ds) p@(y, x) =
      let n = (y - 1, x)
          s = (y + 1, x)
          w = (y, x - 1)
          e = (y, x + 1)
          ne = (y - 1, x + 1)
          nw = (y - 1, x - 1)
          se = (y + 1, x + 1)
          sw = (y + 1, x - 1)
          dirs = [n, s, w, e, ne, nw, se, sw]
          p' =
            case d of
              N -> [n, ne, nw]
              S -> [s, sw, se]
              E -> [e, se, ne]
              W -> [w, nw, sw]
       in case (all (\x -> not (S.member x b)) dirs, any (\x -> S.member x b) p') of
            (True, _) -> Nothing
            (False, True) -> proposeMove ds p
            (False, False) -> Just (p, head p')

rounds :: Board -> [Board]
rounds = go [N, S, W, E]
  where
    go :: [Dir] -> Board -> [Board]
    go d b = b : (go (tail d ++ [head d]) (step d b))

score (Board b) =
  let k = S.elems b
      min_x = minimum (k ^.. folded . _2)
      min_y = minimum (k ^.. folded . _1)
      max_x = maximum (k ^.. folded . _2)
      max_y = maximum (k ^.. folded . _1)
      area = (max_x - min_x + 1) * (max_y - min_y + 1)
   in area - (S.size b)

complete :: Board -> Int
complete b = go 1 (rounds b)
  where
    go n (b1 : b2 : bs) = if b1 == b2 then n else go (n + 1) (b2 : bs)

part1 = score ((rounds input) !! 10)

part2 = complete input
