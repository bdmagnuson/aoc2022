module Aoc2022.Day04
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P

data Range = Range Integer Integer deriving (Show)

data Pair = Pair Range Range deriving (Show)

parser = many (pPair <* P.endOfLine)
  where
    pPair = do
      p1 <- pRange
      P.char ','
      p2 <- pRange
      return $ Pair p1 p2
    pRange = do
      l <- P.decimal
      P.char '-'
      r <- P.decimal
      return $ Range l r

input = getInput "input/day04.txt" parser

contains :: Pair -> Bool
contains (Pair (Range l1 r1) (Range l2 r2)) =
  l1 >= l2 && r1 <= r2 || l2 >= l1 && r2 <= r1

overlap :: Pair -> Bool
overlap p@(Pair (Range l1 r1) (Range l2 r2)) =
  contains p || (l1 <= l2 && r1 >= l2) || (l1 <= r2 && r1 >= r2)

part1 = length . filter contains $ input

part2 = length . filter overlap $ input
