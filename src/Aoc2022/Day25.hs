module Aoc2022.Day25
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P

data Digit = M2 | M1 | Z | P1 | P2 deriving (Eq)

instance Show Digit where
  show M2 = "="
  show M1 = "-"
  show Z = "0"
  show P1 = "1"
  show P2 = "2"

type Number = [Digit]

parser = P.sepBy pLine P.endOfLine
  where
    pLine = P.many1 pDigit
    pDigit =
      (M2 <$ P.char '=')
        <|> (M1 <$ P.char '-')
        <|> (Z <$ P.char '0')
        <|> (P1 <$ P.char '1')
        <|> (P2 <$ P.char '2')

i2s :: Int -> Number
i2s s =
  let diff = reverse (0 : scanl1 (+) (takeWhile (< s) (map (* 2) (iterate (* 5) 1))))
      cycle = reverse $ take (length diff) (iterate (* 5) 1)
      fixup = reverse $ take (length diff) (True : repeat False)
   in zipWith3 (f s) diff cycle fixup

f s d c l =
  if l
    then case ((s - d) `div` c) `mod` 5 of
      0 -> Z
      1 -> P1
      2 -> P2
      3 -> M2
      4 -> M1
    else case ((s - d - 1) `div` c) `mod` 5 of
      0 -> P1
      1 -> P2
      2 -> M2
      3 -> M1
      4 -> Z

s2i :: Number -> Int
s2i = foldl f 0
  where
    f acc d = acc * 5 + dig d
    dig M2 = -2
    dig M1 = -1
    dig Z = 0
    dig P1 = 1
    dig P2 = 2

input = getInput "input/day25.txt" parser

-- 2-=102--02--=1-12=22

part1 = i2s (sum (map s2i input))

part2 = undefined
