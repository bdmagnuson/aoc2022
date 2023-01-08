module Aoc2022.Day20
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.Vector (Vector)
import Data.Vector qualified as V

input :: Vector Int
input = getInput "input/day20.txt" (V.fromList <$> P.sepBy (P.signed P.decimal) P.endOfLine)

-- V.slice except l/r instead of l/len
vslice :: Int -> Int -> V.Vector a -> V.Vector a
vslice l r v =
  if r < l
    then V.empty
    else V.slice l (r - l + 1) v

mix :: Vector Int -> Vector Int -> Vector Int
mix v rng = V.foldl' go rng (V.fromList [0 .. V.length v - 1])
  where
    go v' n =
      let (Just idx) = V.findIndex (== n) v'
       in moveV v' idx (v V.! n)

moveV :: Vector Int -> Int -> Int -> Vector Int
moveV v f s =
  let len = V.length v
      t' = (f + s) `mod` (len - 1)
      t = if t' == 0 then len - 1 else t'
   in if t > f
        then V.concat [vslice 0 (f - 1) v, vslice (f + 1) t v, V.singleton (v V.! f), vslice (t + 1) (V.length v - 1) v]
        else V.concat [vslice 0 (t - 1) v, V.singleton (v V.! f), vslice t (f - 1) v, vslice (f + 1) (V.length v - 1) v]

answer vec res =
  let c = cycle . V.toList . V.map (vec V.!) $ res
      c' = dropWhile (/= 0) c
   in (c' !! 1000) + (c' !! 2000) + (c' !! 3000)

part1 = answer input (mix input (V.fromList [0 .. V.length input - 1]))

part2 =
  let key = V.map (* 811589153) input
   in answer key (iterate (mix key) (V.fromList [0 .. V.length input - 1]) !! 10)
