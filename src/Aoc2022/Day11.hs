module Aoc2022.Day11
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens hiding (op)
import Control.Monad.State.Strict
import Data.Attoparsec.Text qualified as P

data Monkey = Monkey
  { _items :: [Integer],
    _op :: Integer -> Integer,
    _dest :: Integer -> Integer,
    _handled :: Int,
    _divisor :: Integer
  }

makeLenses ''Monkey

data Part = Part1 | Part2 deriving (Eq)

data Rhs = Dig Integer | Old

newtype St = St {_monkeys :: [Monkey]}

makeLenses ''St

parser = P.sepBy1 pMonkey P.endOfLine
  where
    pMonkey = do
      P.string "Monkey " >> P.decimal >> P.char ':' >> P.endOfLine
      items <- do
        P.string "  Starting items: "
        x <- P.sepBy1 P.decimal (P.string ", ")
        P.endOfLine
        return x
      op <- do
        P.string "  Operation: new = old "
        c <- P.anyChar
        P.char ' '
        rhs <- (Dig <$> P.decimal) <|> (Old <$ P.string "old")
        P.endOfLine
        return $ case (c, rhs) of
          ('+', Dig x) -> (+x)
          ('+', Old) -> (*2)
          ('*', Dig x) -> (*x)
          ('*', Old) -> \y -> y * y
      (dest, div) <- do
        P.string "  Test: divisible by "
        d <- P.decimal <* P.endOfLine
        P.string "    If true: throw to monkey "
        t <- P.decimal <* P.endOfLine
        P.string "    If false: throw to monkey "
        f <- P.decimal <* P.endOfLine
        return (\x -> if x `mod` d == 0 then t else f, d)
      return $ Monkey items op dest 0 div

input = getInput "input/day11.txt" parser

toss :: Part -> Int -> State St ()
toss p idx = do
  m <- uses monkeys (^?! ix idx)
  monkeys . ix idx . items .= []
  mapM_ (f m) (m ^. items)
  where
    f :: Monkey -> Integer -> State St ()
    f m i = do
      let i' =
            if p == Part2
              then (m ^. op $ i) `mod` modulus
              else (m ^. op $ i) `div` 3
      let m' = (m ^. dest) i'
      monkeys . ix idx . handled += 1
      monkeys . ix (fromIntegral m') . items %= (\x -> x ++ [i'])

doRound :: Part -> St -> St
doRound p ms = execState (traverse (toss p) [0 .. length (ms ^. monkeys) - 1]) ms

play :: Part -> Int -> St
play p i = iterate (doRound p) (St input) !! i

modulus = foldl1 lcm (map (view divisor) input)

score = product . take 2 . revsort . map (view handled) . view monkeys

part1 = score (play Part1 20)

part2 = score (play Part2 10000)
