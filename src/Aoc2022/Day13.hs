module Aoc2022.Day13
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.List

data Elem = One Int | More [Elem] deriving (Show, Eq)

instance Ord Elem where
  compare (One l) (One r) = compare l r
  compare (More l) (More r) =
    case dropWhile (== EQ) (zipWith compare l r) of
      [] -> compare (length l) (length r)
      (LT : _) -> LT
      (GT : _) -> GT
  compare (One l) r = compare (More [One l]) r
  compare l (One r) = compare l (More [One r])

pInput = P.sepBy1 pPair P.endOfLine

pPair = do
  p1 <- pPacket <* P.endOfLine
  p2 <- pPacket <* P.endOfLine
  return (p1, p2)

pPacket = More <$> pElem
  where
    pElem = do
      P.char '['
      d <- P.sepBy (One <$> P.decimal <|> More <$> pElem) (P.char ',')
      P.char ']'
      return d

input1 = getInput "input/day13.txt" pInput

input2 = sort (div1 : div2 : concatMap (\(a, b) -> [a, b]) input1)

(Right div1) = P.parseOnly pPacket "[[2]]"

(Right div2) = P.parseOnly pPacket "[[6]]"

part1 = sum . map fst . filter (\(_, (a, b)) -> a < b) $ zip [1 ..] input1

part2 =
  let (Just idx1) = elemIndex div1 input2
      (Just idx2) = elemIndex div2 input2
   in (idx1 + 1) * (idx2 + 1)
