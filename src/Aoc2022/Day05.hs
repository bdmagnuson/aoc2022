module Aoc2022.Day05
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.List (splitAt, transpose)
import Data.Maybe (catMaybes)
import Data.Text (Text)

data Input = Input
  { stack :: [[Char]],
    move :: [(Int, Int, Int)]
  }
  deriving (Show)

parser = do
  s <- pStack
  i <- pIds
  P.endOfLine
  m <- many pMove
  return $ Input s m
  where
    pIds = P.char ' ' *> P.takeWhile1 (P.inClass " 123456789") >> P.endOfLine
    pStack = (fmap catMaybes . transpose) <$> many ((P.sepBy1 (pLetter <|> pNull) (P.char ' ')) <* P.endOfLine)
    pMove = do
      q <- P.string "move " *> P.decimal
      f <- subtract 1 <$> (P.string " from " *> P.decimal)
      t <- subtract 1 <$> (P.string " to " *> P.decimal)
      P.endOfLine
      return (q, f, t)
    pLetter = do
      P.char '['
      c <- P.anyChar
      P.char ']'
      return (Just c)
    pNull = do
      P.string "   "
      return Nothing

input = getInput "input/day05.txt" parser

step op a (q, f, t) =
  let (h, r) = splitAt q (a !! f)
   in a & ix f .~ r
        & ix t .~ (op h ++ (a !! t))

part1 = map head $ foldl (step reverse) (stack input) (move input)

part2 = map head $ foldl (step id) (stack input) (move input)
