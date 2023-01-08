module Aoc2022.Day22
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens hiding (Empty, elements)
import Data.Attoparsec.Text qualified as P
import Data.Map (Map)
import Data.Map qualified as M
import Test.QuickCheck

data Elem = Null | Empty | Wall deriving (Show, Eq, Ord)

data Instr = Walk Integer | L | R deriving (Eq, Show)

data Dir = N | S | E | W deriving (Eq, Show, Enum, Bounded)

parser = do
  map <- f <$> P.sepBy pLine P.endOfLine
  many P.endOfLine
  dir <- P.many1 ((Walk <$> P.decimal) <|> (L <$ P.char 'L') <|> (R <$ P.char 'R'))
  return (map, dir)
  where
    pLine = P.many1 ((Null <$ P.char ' ') <|> (Empty <$ P.char '.') <|> (Wall <$ P.char '#'))
    f m = M.fromList . filter (\(_, d) -> d /= Null) . concat $ zipWith (\y r -> zipWith (\x e -> ((y, x), e)) [0 ..] r) [0 ..] m

(board, dir) = getInput "input/day22.txt" parser

board2cube :: Map (Integer, Integer) Elem -> Map (Integer, Integer, Integer) Elem
board2cube m = M.fromList (concat [f1, f2, f3, f4, f5, f6])
  where
    f1 = [((1, y - 100, x - 50), m ^?! ix (y, x)) | y <- [100 .. 149], x <- [50 .. 99]]
    f2 = [((2, y, x - 100), m ^?! ix (y, x)) | y <- [0 .. 49], x <- [100 .. 149]]
    f3 = [((3, y - 150, x), m ^?! ix (y, x)) | y <- [150 .. 199], x <- [0 .. 49]]
    f4 = [((4, y - 50, x - 50), m ^?! ix (y, x)) | y <- [50 .. 99], x <- [50 .. 99]]
    f5 = [((5, y - 100, x), m ^?! ix (y, x)) | y <- [100 .. 149], x <- [0 .. 49]]
    f6 = [((6, y, x - 50), m ^?! ix (y, x)) | y <- [0 .. 49], x <- [50 .. 99]]

cube = board2cube board

prop_reverse = do
  s <- choose (1, 6)
  y <- elements [0, 1, 2, 47, 48, 49]
  x <- elements [0, 1, 2, 47, 48, 49]
  d <- elements [N, W, E, S]
  let (p', d') = step (s, y, x) d
  let (p'', d'') = step p' (turn d')
  return $ ((s, y, x) == p'') && (d == turn d'')
  where
    turn = \case
      N -> S
      S -> N
      E -> W
      W -> E

step :: (Integer, Integer, Integer) -> Dir -> ((Integer, Integer, Integer), Dir)
step (1, 0, x) N = ((4, 49, x), N)
step (2, 0, x) N = ((3, 49, x), N)
step (3, 0, x) N = ((5, 49, x), N)
step (4, 0, x) N = ((6, 49, x), N)
step (5, 0, x) N = ((4, x, 0), E)
step (6, 0, x) N = ((3, x, 0), E)
step (1, 49, x) S = ((3, x, 49), W)
step (2, 49, x) S = ((4, x, 49), W)
step (3, 49, x) S = ((2, 0, x), S)
step (4, 49, x) S = ((1, 0, x), S)
step (5, 49, x) S = ((3, 0, x), S)
step (6, 49, x) S = ((4, 0, x), S)
step (1, y, 49) E = ((2, 49 - y, 49), W)
step (2, y, 49) E = ((1, 49 - y, 49), W)
step (3, y, 49) E = ((1, 49, y), N)
step (4, y, 49) E = ((2, 49, y), N)
step (5, y, 49) E = ((1, y, 0), E)
step (6, y, 49) E = ((2, y, 0), E)
step (1, y, 0) W = ((5, y, 49), W)
step (2, y, 0) W = ((6, y, 49), W)
step (3, y, 0) W = ((6, 0, y), S)
step (4, y, 0) W = ((5, 0, y), S)
step (5, y, 0) W = ((6, 49 - y, 0), E)
step (6, y, 0) W = ((5, 49 - y, 0), E)
step (f, y, x) S = ((f, y + 1, x), S)
step (f, y, x) N = ((f, y - 1, x), N)
step (f, y, x) W = ((f, y, x - 1), W)
step (f, y, x) E = ((f, y, x + 1), E)

coords :: [(Integer, Integer)]
coords = M.keys board

maxX = maximum (coords ^.. folded . _2)

maxY = maximum (coords ^.. folded . _1)

yEdges :: [(Integer, Integer)]
yEdges = map f [0 .. maxY - 1]
  where
    f y =
      let row = map snd $ filter (\(y', _) -> y' == y) coords
       in (minimum row, maximum row)

xEdges :: [(Integer, Integer)]
xEdges = map f [0 .. maxX - 1]
  where
    f x =
      let col = map fst $ filter (\(_, x') -> x' == x) coords
       in (minimum col, maximum col)

instr2dir :: [Instr] -> Dir -> [Dir]
instr2dir [] _ = []
instr2dir (i : is) d =
  case (i, d) of
    (L, N) -> instr2dir is W
    (L, S) -> instr2dir is E
    (L, E) -> instr2dir is N
    (L, W) -> instr2dir is S
    (R, N) -> instr2dir is E
    (R, S) -> instr2dir is W
    (R, E) -> instr2dir is S
    (R, W) -> instr2dir is N
    (Walk n, _) -> replicate (fromIntegral n) d ++ instr2dir is d

walk2 :: ((Integer, Integer, Integer), Dir)
walk2 = go ((6, 0, 0), E) dir
  where
    go p [] = p
    go (p, d) (L : is) =
      case d of
        N -> go (p, W) is
        S -> go (p, E) is
        E -> go (p, N) is
        W -> go (p, S) is
    go (p, d) (R : is) =
      case d of
        N -> go (p, E) is
        S -> go (p, W) is
        E -> go (p, S) is
        W -> go (p, N) is
    go p ((Walk 0) : is) = go p is
    go (p, d) (Walk n : is) =
      let (p', d') = step p d
       in case cube ^? ix p' of
            Nothing -> error "wtf"
            Just Empty -> go (p', d') (Walk (n - 1) : is)
            Just Wall -> go (p, d) (Walk (n - 1) : is)

walk :: (Integer, Integer)
walk = foldl go (1, fst $ head yEdges) (instr2dir dir E)
  where
    go (y, x) d =
      let foo = case d of
            N -> case board ^? ix (y - 1, x) of
              Just Wall -> (y, x)
              Just Empty -> (y - 1, x)
              Nothing ->
                let p = (snd (xEdges !! fromIntegral x), x)
                 in case board ^? ix p of
                      Just Wall -> (y, x)
                      Just Empty -> p
            S -> case board ^? ix (y + 1, x) of
              Just Wall -> (y, x)
              Just Empty -> (y + 1, x)
              Nothing ->
                let p = (fst (xEdges !! fromIntegral x), x)
                 in case board ^? ix p of
                      Just Wall -> (y, x)
                      Just Empty -> p
            E -> case board ^? ix (y, x + 1) of
              Just Wall -> (y, x)
              Just Empty -> (y, x + 1)
              Nothing ->
                let p = (y, fst (yEdges !! fromIntegral y))
                 in case board ^? ix p of
                      Just Wall -> (y, x)
                      Just Empty -> p
            W -> case board ^? ix (y, x - 1) of
              Just Wall -> (y, x)
              Just Empty -> (y, x - 1)
              Nothing ->
                let p = (y, snd (yEdges !! fromIntegral y))
                 in case board ^? ix p of
                      Just Wall -> (y, x)
                      Just Empty -> p
       in foo

score y x d = 1000 * y + 4 * x + d'
  where
    d' = case d of
      E -> 0
      S -> 1
      W -> 2
      N -> 3

part1 =
  let (y, x) = walk
      d = last (instr2dir dir E)
   in score (y + 1) (x + 1) d

part2 =
  let ((f, y, x), d) = walk2
   in case f of
        1 -> score ((y + 1) + 100) ((x + 1) + 50) d
        2 -> score ((y + 1) + 0) ((x + 1) + 100) d
        3 -> score ((y + 1) + 150) ((x + 1) + 0) d
        4 -> score ((y + 1) + 50) ((x + 1) + 50) d
        5 -> score ((y + 1) + 100) ((x + 1) + 0) d
        6 -> score ((y + 1) + 0) ((x + 1) + 50) d
