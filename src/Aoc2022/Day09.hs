module Aoc2022.Day09
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Attoparsec.Text qualified as P
import Data.Functor
import Data.Map qualified as M
import Data.Set qualified as S

data Move = R | L | U | D deriving (Show)

type Pt = (Int, Int)

data St = St
  { _knot :: [Pt],
    _visited :: S.Set Pt
  }
  deriving (Show)

input :: [Move]
input = getInput "input/day09.txt" parser

parser = concat <$> many (pMove <* P.endOfLine)
  where
    pMove = do
      d <- (R <$ P.char 'R') <|> (L <$ P.char 'L') <|> (D <$ P.char 'D') <|> (U <$ P.char 'U')
      P.char ' '
      s <- P.decimal
      return $ replicate s d

makeLenses ''St

moveHead :: Move -> State St ()
moveHead m = do
  case m of
    R -> update 0 1 0
    L -> update 0 (-1) 0
    U -> update 0 0 1
    D -> update 0 0 (-1)
  len <- uses knot length
  mapM_ follow [0 .. len - 2]
  t <- use knot
  visited %= S.insert (last t)
  where
    follow i = do
      (hx, hy) <- uses knot (^?! ix i)
      (tx, ty) <- uses knot (^?! ix (i + 1))
      let dx = hx - tx
      let dy = hy - ty
      when (abs dx > 1 || abs dy > 1) (update (i + 1) (signum dx) (signum dy))
    update i dx dy = do
      knot . ix i . _1 += dx
      knot . ix i . _2 += dy

solve x = S.size (view visited (execState (traverse moveHead input) (initialSt x)))
  where
    initialSt x = St (replicate x (0, 0)) (S.singleton (0, 0))

part1 = solve 2

part2 = solve 10
