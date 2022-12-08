module Aoc2022.Day07
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Comonad
import Control.Comonad.Cofree
import Control.Lens hiding ((:<))
import Control.Monad.State.Strict
import Data.Attoparsec.Text qualified as P
import Data.Eq.Deriving
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Text.Show.Deriving

data Cmd = Up | Down Text | Ls [TraceEntry] deriving (Show)

data TraceEntry
  = TraceDir Text
  | TraceFile Text Int
  deriving (Show)

data TreeF a = TreeF
  { _dirs :: Map Text a,
    _files :: Map Text Int
  }
  deriving (Functor, Show)

type Tree a = Cofree TreeF a

$(deriveShow1 ''TreeF)
$(deriveEq1 ''TreeF)

makeLenses ''TreeF

type Crumb a = (Tree a, Text)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

parser = do
  P.string "$ cd /\n"
  many (pCd <|> pLs)
  where
    pCd = do
      P.string "$ cd "
      (P.string "..\n" >> return Up) <|> (Down <$> pRest)
    pLs = do
      P.string "$ ls\n"
      Ls <$> many (pDir <|> pFile)
    pDir = TraceDir <$> (P.string "dir " *> pRest)
    pFile = do
      size <- P.decimal
      P.char ' '
      name <- pRest
      return $ TraceFile name size
    pRest = P.takeWhile (/= '\n') <* P.endOfLine

input = getInput "input/day07.txt" parser

enter :: Tree a -> Zipper a
enter t = (t, [])

exit :: Zipper a -> Tree a
exit (t, _) = t

down :: Text -> Zipper a -> Zipper a
down n ((x@(_ :< TreeF d f), b)) = (d ^?! ix n, (x, n) : b)

emptyDir = () :< (TreeF M.empty M.empty)

up :: Zipper a -> Zipper a
up (t, []) = (t, [])
up (t, (a :< TreeF d f, n) : bs) = (a :< TreeF (d & at n .~ Just t) f, bs)

toRoot :: Zipper a -> Zipper a
toRoot z@(_, []) = z
toRoot z = toRoot (up z)

adjust f (a :< t, b) = (a :< f t, b)

data St = St {_tree :: Zipper ()} deriving (Show)

makeLenses ''St

f :: Cmd -> State St ()
f Up = tree %= up
f (Down d) = tree %= down d
f (Ls t) = mapM_ f t
  where
    f :: TraceEntry -> State St ()
    f (TraceDir d) = tree %= adjust (dirs . at d .~ Just emptyDir)
    f (TraceFile n s) = tree %= adjust (files . at n .~ Just s)

mkTree =
  let st = execState (traverse f input) (St (enter (() :< TreeF M.empty M.empty)))
   in exit (toRoot (st ^. tree))

sumFiles :: Tree () -> Tree Int
sumFiles s = extend f s
  where
    f :: Tree () -> Int
    f (() :< TreeF d f) =
      let dsum = sum (map (extract . sumFiles) (M.elems d)) :: Int
       in dsum + sum (M.elems f)

collectSizes :: Tree Int -> Tree [Int]
collectSizes s = let f (a :< TreeF d _) = a : (concat (map (extract . collectSizes) (M.elems d))) in extend f s

annTree = sumFiles mkTree

dirSizes = extract (collectSizes annTree)

toFree = 30000000 - (70000000 - extract annTree)

part1 = sum (filter (<= 100000) dirSizes)

part2 = minimum (filter (>= toFree) dirSizes)
