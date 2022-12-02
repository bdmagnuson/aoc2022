module Aoc2022.Day02
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P

data Sign = Rock | Paper | Scissors deriving (Show)

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
score (s1, s2) = outcome + play
  where
    outcome =
      case (s1, s2) of
        (Rock, Paper) -> 6
        (Rock, Rock) -> 3
        (Rock, Scissors) -> 0
        (Paper, Scissors) -> 6
        (Paper, Paper) -> 3
        (Paper, Rock) -> 0
        (Scissors, Rock) -> 6
        (Scissors, Scissors) -> 3
        (Scissors, Paper) -> 0
    play =
      case s2 of
        Rock -> 1
        Paper -> 2
        Scissors -> 3

adjust :: (Sign, Outcome) -> (Sign, Sign)
adjust (s1, s2) = (s1, fixed)
  where
    fixed =
      case (s1, s2) of
        (Rock, Lose) -> Scissors
        (Rock, Draw) -> Rock
        (Rock, Win) -> Paper
        (Paper, Lose) -> Rock
        (Paper, Draw) -> Paper
        (Paper, Win) -> Scissors
        (Scissors, Lose) -> Paper
        (Scissors, Draw) -> Scissors
        (Scissors, Win) -> Rock

convert f g = map (\(x, y) -> (f x, g y))

part1 = sum (map score (convert toSign toSign input))

part2 = sum (map (score . adjust) (convert toSign toOutcome input))
