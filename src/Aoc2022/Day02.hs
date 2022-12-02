module Aoc2022.Day02
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P

data Sign = Rock | Paper | Scissors

data Outcome = Win | Lose | Draw

parser = many (play <* P.endOfLine)
  where
    play = do
      c1 <- P.satisfy (P.inClass "ABC")
      P.char ' '
      c2 <- P.satisfy (P.inClass "XYZ")
      return (c1, c2)

input = getInput "input/day02.txt" parser

toSign = \case
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors

toOutcome = \case
  'X' -> Lose
  'Y' -> Draw
  'Z' -> Win

score :: (Sign, Sign) -> Int
score (Rock, Rock) = 3 + 1
score (Rock, Paper) = 6 + 2
score (Rock, Scissors) = 0 + 3
score (Paper, Rock) = 0 + 1
score (Paper, Paper) = 3 + 2
score (Paper, Scissors) = 6 + 3
score (Scissors, Rock) = 6 + 1
score (Scissors, Paper) = 0 + 2
score (Scissors, Scissors) = 3 + 3

adjust :: Sign -> Outcome -> Sign
adjust Rock Lose = Scissors
adjust Rock Draw = Rock
adjust Rock Win = Paper
adjust Paper Lose = Rock
adjust Paper Draw = Paper
adjust Paper Win = Scissors
adjust Scissors Lose = Paper
adjust Scissors Draw = Scissors
adjust Scissors Win = Rock

convert f g = map (\(x, y) -> (f x, g y))

part1 = sum (map score (convert toSign toSign input))

part2 = sum (map (score . \(x, y) -> (x, adjust x y)) (convert toSign toOutcome input))
