module Aoc2022.Day06
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.List (nub)
import Data.Text (unpack)

input = getInput "input/day06.txt" (unpack <$> P.takeText)

marker :: Int -> [Char] -> Int
marker z s = let (xs, ys) = splitAt z s in go (reverse xs) z ys
  where
    go k r (x : xs) = if length (nub k) == z then r else go (x : init k) (r + 1) xs

part1 = marker 4 input

part2 = marker 14 input
