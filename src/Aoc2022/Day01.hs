module Aoc2022.Day01
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.List (sortOn)
import Data.Ord (Down (..))

parser = P.sepBy1 (many (P.decimal <* P.endOfLine)) P.endOfLine

input = getInput "input/day01.txt" parser

part1 = maximum (map sum input)

part2 = sum . take 3 . sortOn Down $ map sum input
