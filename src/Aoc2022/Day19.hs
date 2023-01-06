module Aoc2022.Day19
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Debug.Trace

data Material = Ore | Clay | Obsidian | Geode deriving (Show, Ord, Eq)

data St = St
  { _time :: Integer,
    _total :: Map Material Integer,
    _rate :: Map Material Integer
  }
  deriving (Show)

makeLenses ''St

type Recipie = Map Material (Map Material Integer)

data Tree a = Node [Tree a] | Leaf a deriving (Show)

makeBaseFunctor ''Tree

input = getInput "input/day19.txt" parser

parser = P.sepBy pLine P.endOfLine

pLine = do
  P.string "Blueprint "
  P.decimal
  P.string ": Each ore robot costs "
  ore <- P.decimal
  P.string " ore. Each clay robot costs "
  clay <- P.decimal
  P.string " ore. Each obsidian robot costs "
  obsidian_ore <- P.decimal
  P.string " ore and "
  obsidian_clay <- P.decimal
  P.string " clay. Each geode robot costs "
  geode_ore <- P.decimal
  P.string " ore and "
  geode_obsidian <- P.decimal
  P.string " obsidian."
  return $
    M.fromList
      [ (Ore, M.fromList [(Ore, ore)]),
        (Clay, M.fromList [(Ore, clay)]),
        (Obsidian, M.fromList [(Ore, obsidian_ore), (Clay, obsidian_clay)]),
        (Geode, M.fromList [(Ore, geode_ore), (Obsidian, geode_obsidian)])
      ]

initState :: Integer -> St
initState x = St x (M.fromList (zip m [0, 0, 0, 0])) (M.fromList (zip m [1, 0, 0, 0]))
  where
    m = [Ore, Clay, Obsidian, Geode]

down :: Recipie -> St -> TreeF St St
down r s
  | s ^. time == 0 = LeafF s
  | otherwise = NodeF options
  where
    turn s =
      s & time -~ 1
        & total %~ M.unionWith (+) (s ^. rate)
    options = catMaybes [build Ore, build Clay, build Obsidian, build Geode]
    build :: Material -> Maybe St
    build m
      | any (\m' -> s ^?! rate . ix m' == 0) (M.keys (r ^?! ix m)) = Nothing
      | liftA2 (>) (s ^? rate . ix m) (max_rate ^? ix m) == Just True = Nothing
      | otherwise =
          let s' = head (dropWhile (\x -> notEnough x && x ^. time > 0) (iterate turn s))
           in if s' ^. time == 0
                then Just s'
                else Just (update . turn $ s')
      where
        update s' =
          s' & rate . ix m +~ 1
            & total %~ M.union (remain s')
        notEnough s = any (< 0) (M.elems (remain s))
        remain s' = M.intersectionWith (-) (s' ^. total) (r ^?! ix m)
    max_rate = foldl1 (M.unionWith max) (M.elems r)

up (LeafF s) = s ^?! total . ix Geode
up (NodeF s) = maximum s

quality :: Integer -> Recipie -> Integer
quality x r = hylo up (down r) (initState x)

part1 = sum $ zipWith (*) [1 ..] (map (quality 24) input)

part2 = product $ map (quality 32) (take 3 input)

-- ans = sum $ zipWith (*) [1 ..] [1, 1, 0, 0, 1, 5, 9, 2, 1, 4, 0, 4, 1, 12, 8, 0, 0, 5, 5, 0, 1, 5, 5, 0, 15, 0, 7, 3, 0, 8]
