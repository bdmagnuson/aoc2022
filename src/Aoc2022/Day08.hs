module Aoc2022.Day08
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Map qualified as M

input :: [[Int]]
input = getInput "input/day08.txt" parser

parser = many (many (read . pure <$> P.digit) <* P.endOfLine)

height = length input

width = length (head input)

pts = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

grid = M.fromList (zip pts (concat input))

vNorth (x, y) = [(x, y') | y' <- [y - 1, y - 2 .. 0]]

vSouth (x, y) = [(x, y') | y' <- [y + 1 .. height - 1]]

vEast (x, y) = [(x', y) | x' <- [x - 1, x - 2 .. 0]]

vWest (x, y) = [(x', y) | x' <- [x + 1 .. width - 1]]

shorter :: (Int, Int) -> (Int, Int) -> Bool
shorter p a = grid ^?! ix a < grid ^?! ix p

visable :: (Int, Int) -> Bool
visable p = or (map (all (shorter p)) [vNorth p, vWest p, vEast p, vSouth p])

scenic :: (Int, Int) -> Int
scenic p = product (map f [vNorth p, vWest p, vEast p, vSouth p])
  where
    f x = add $ map (shorter p) x
    add [] = 0
    add (False : _) = 1
    add (True : xs) = 1 + add xs

part1 = sum (map ((\x -> if x then 1 else 0) . visable) pts)

part2 = maximum (map scenic pts)
