module Aoc2022.Day03
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.Char (chr, isAlpha, ord)
import Data.List.Split (chunksOf)
import Data.Set qualified as S
import Data.Text qualified as T

type Compartment = S.Set Char

data Sack = Sack Compartment Compartment deriving (Show)

parser = many (pSack <* P.endOfLine)
  where
    pSack = do
      line <- T.unpack <$> P.takeWhile isAlpha
      let (l, r) = splitAt (length line `div` 2) line
      return $ Sack (S.fromList l) (S.fromList r)

input = getInput "input/day03.txt" parser

pri :: Char -> Int
pri c =
  let la = ord 'a'
      ca = ord 'A'
      o = ord c
   in if o >= la then 1 + (o - la) else 27 + (o - ca)

part1 =
  let score (Sack l r) = pri . head . S.toList $ S.intersection l r
   in sum (map score input)

part2 = sum (map (pri . f) (chunksOf 3 input))
  where
    f [Sack l1 r1, Sack l2 r2, Sack l3 r3] =
      let s1 = S.union l1 r1
          s2 = S.union l2 r2
          s3 = S.union l3 r3
          d1 = S.intersection s1 s2
          d2 = S.intersection s2 s3
          d3 = S.intersection s1 s3
          d4 = S.intersection d1 d2
          d5 = S.intersection d1 d3
          d6 = S.intersection d4 d5
       in head (S.toList d6)
