module Aoc2022.Day17
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Attoparsec.Text qualified as P
import Data.List (sort, sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector qualified as V
import Debug.Trace
import Statistics.Autocorrelation

data Dir = L | R deriving (Show)

parser = many ((L <$ P.char '<') <|> (R <$ P.char '>'))

input = getInput "input/day17.txt" parser

type Rock = Set (Integer, Integer)

data Shape = H4 | Cross | Ell | V4 | Square deriving (Enum, Eq, Bounded)

data St = St
  { _fixed :: Rock,
    _falling :: Rock,
    _wind :: NonEmpty Dir,
    _nextShape :: NonEmpty Shape,
    _total :: Integer
  }

makeLenses ''St

instance Show St where
  show s =
    let (Just ceiling) = maximumOf (folded . _2) (S.union (s ^. fixed) (s ^. falling))
     in unlines (map row [ceiling, ceiling - 1 .. 0] ++ ["+-------+"])
    where
      row r = concat [show r, "|", map (point r) [0 .. 6], "|"]
      point r c
        | S.member (c, r) (s ^. fixed) = '#'
        | S.member (c, r) (s ^. falling) = '@'
        | otherwise = '.'

notNull = not . S.null

push :: State St ()
push = do
  (d :| ds) <- use wind
  rock <- use falling
  f <- use fixed
  wind .= head ds :| tail ds
  let lrock = S.map (\(x, y) -> (x - 1, y)) rock
  let rrock = S.map (\(x, y) -> (x + 1, y)) rock
  case d of
    L -> do
      if any (< 0) (lrock ^.. folded . _1) || (notNull (S.intersection lrock f))
        then return ()
        else falling .= lrock
    R -> do
      if any (== 7) (rrock ^.. folded . _1) || (notNull (S.intersection rrock f))
        then return ()
        else falling .= rrock

fall :: State St Integer
fall = do
  rock <- use falling
  f <- use fixed
  let drock = S.map (\(x, y) -> (x, y - 1)) rock
  if any (< 0) (drock ^.. folded . _2) || (notNull (S.intersection drock f))
    then do
      fixed %= S.union rock
      newRock
    else falling .= drock
  use total >>= return

rockHeight :: St -> Integer
rockHeight s = (fromJust $ maximumOf (folded . _2) (s ^. fixed)) + 1

newRock :: State St ()
newRock = do
  c <- uses fixed (fromMaybe (-1) . (maximumOf (folded . _2)))
  (s :| ss) <- use nextShape
  nextShape .= head ss :| tail ss
  let ns = case s of
        H4 -> S.fromList [(2, c + 4), (3, c + 4), (4, c + 4), (5, c + 4)]
        Cross -> S.fromList [(2, c + 5), (3, c + 5), (4, c + 5), (3, c + 4), (3, c + 6)]
        Ell -> S.fromList [(2, c + 4), (3, c + 4), (4, c + 4), (4, c + 5), (4, c + 6)]
        V4 -> S.fromList [(2, c + 4), (2, c + 5), (2, c + 6), (2, c + 7)]
        Square -> S.fromList [(2, c + 4), (2, c + 5), (3, c + 4), (3, c + 5)]
  total += 1
  falling .= ns

initState =
  let (w : ws) = input
      wind = (NE.cycle (w :| ws))
      shapes = (NE.cycle (H4 :| [Cross, Ell, V4, Square]))
   in St S.empty S.empty wind shapes 0

step :: St -> St
step = execState (push >> fall)

s1 = execState newRock initState

runRocks = drop 1 (iterate nextRock s1)

nextRock s = head . dropWhile (\s' -> s' ^. total == s ^. total) $ iterate step s

part1 = last (map rockHeight (take 2022 runRocks))

part2 = foo 1000000000000

-- Test input parameters
-- leadin = 15 :: Int

-- period = 35 :: Int

-- By inspection it looked as though the height of the tower
-- eventually grows periodically after some leadin number of blocks.
-- Getting the period was determined with an autocorrelation of
-- function of differences between each stone dropping.  The leadin
-- period was determined emperically by manually searching the
-- function j until I found the lowest input that returned a constant
-- value.  There's surely a smarter way of doing this.

leadin = 641 :: Int

period = 1700 :: Int

foo :: Integer -> Integer
foo x =
  let aaa = map rockHeight (take (leadin + period) runRocks)
      (f, s) = splitAt leadin aaa
      start = last f
      extra = map (\x -> x - (last f)) s
      (d, m) = (x - (fromIntegral leadin)) `divMod` (fromIntegral period)
   in if m == 0
        then start + d * (last extra)
        else start + d * (last extra) + (extra !! ((fromIntegral m) - 1))

b = map rockHeight (take 5000 runRocks)

c = zipWith (-) b (drop 1 b)

d = (autocorrelation (V.fromList (map fromIntegral c))) ^. _1

e = zip [0 ..] (V.toList d)

f = sortBy (comparing snd) e

g = f !! (length f - 2)

j n = let foo = drop n b in zipWith (-) foo (drop period foo)
