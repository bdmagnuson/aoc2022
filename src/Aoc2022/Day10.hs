module Aoc2022.Day10
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Attoparsec.Text qualified as P
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Debug.Trace

data Reg = RegX deriving (Show, Ord, Eq)

type Event = (Int, Instr)

type Regs = M.Map Reg Int

data Instr
  = Nop
  | Addx Int
  deriving (Show)

newtype St = St {_regs :: Regs} deriving (Show)

makeLenses ''St

input :: [Instr]
input = getInput "input/day10.txt" parser

parser = many (pInstr <* P.endOfLine)
  where
    pInstr = pNop <|> pAddx
    pNop = Nop <$ P.string "noop"
    pAddx = do
      P.string "addx "
      Addx <$> P.signed P.decimal

exec :: Instr -> State St [Int]
exec i = do
  xp <- uses regs (^?! ix RegX)
  case i of
    Nop -> do
      return [xp]
    Addx v -> do
      regs . ix RegX += v
      return [xp, xp + v]

xs = 1 : init (concat $ evalState (traverse exec input) (St (M.fromList [(RegX, 1)])))

part1 = sum . map (\(x, r) -> if (x + 20) `mod` 40 == 0 then x * r else 0) $ zip [1 ..] xs

part2 = mapM_ f (chunksOf 40 xs)
  where
    f x = do
      mapM_ f' (zip [0 ..] x)
      putStrLn ""
    f' (p, s) = if abs (p - s) <= 1 then putStr "#" else putStr "."
