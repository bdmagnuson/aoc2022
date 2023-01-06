module Aoc2022.Day21
  ( part1,
    part2,
  )
where

import Aoc2022.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Char (isAlpha)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text, unpack)

data Expr
  = NumE Int
  | VarE String
  | AddE Expr Expr
  | SubE Expr Expr
  | MulE Expr Expr
  | DivE Expr Expr
  deriving (Show)

data Op = Eq | Mul | Sub | Add | Div deriving (Show)

data Tree = Node String Op Tree Tree | Leaf String Int deriving (Show)

data Dir = L | R deriving (Show)

parser = M.fromList <$> P.sepBy pLine P.endOfLine
  where
    pIdentifier = unpack <$> P.takeWhile isAlpha
    pLine = do
      lhs <- pIdentifier
      P.string ": "
      rhs <- (NumE <$> P.decimal) <|> pExpr
      return (lhs, rhs)
    pExpr = do
      l <- VarE <$> pIdentifier
      P.char ' '
      op <- (AddE <$ P.char '+') <|> (SubE <$ P.char '-') <|> (MulE <$ P.char '*') <|> (DivE <$ P.char '/')
      P.char ' '
      r <- VarE <$> pIdentifier
      return $ op l r

input = getInput "input/day21.txt" parser

buildTree :: String -> Tree
buildTree label =
  case input ^?! ix label of
    NumE n -> Leaf label n
    AddE (VarE l) (VarE r) -> Node label Add (buildTree l) (buildTree r)
    SubE (VarE l) (VarE r) -> Node label Sub (buildTree l) (buildTree r)
    DivE (VarE l) (VarE r) -> Node label Div (buildTree l) (buildTree r)
    MulE (VarE l) (VarE r) -> Node label Mul (buildTree l) (buildTree r)

findStart :: String -> Tree -> Maybe [Dir]
findStart s (Leaf label _) = if label == s then Just [] else Nothing
findStart s (Node label _ l r) =
  if label == s
    then Just []
    else case (findStart s l, findStart s r) of
      (Just l, Nothing) -> Just (L : l)
      (Nothing, Just r) -> Just (R : r)
      (Nothing, Nothing) -> Nothing
      (Just _, Just _) -> error "found both"

eval (Leaf _ i) = i
eval (Node _ Add l r) = eval l + eval r
eval (Node _ Sub l r) = eval l - eval r
eval (Node _ Mul l r) = eval l * eval r
eval (Node _ Div l r) = eval l `div` eval r

derive :: [Dir] -> Tree -> Int -> Int
derive [] (Leaf s _) c = if s == "humn" then c else error "wtf"
derive (d : ds) (Node _ op l r) c =
  case d of
    L ->
      let rhs = eval r
       in case op of
            Eq -> derive ds l rhs
            Add -> derive ds l (c - rhs)
            Sub -> derive ds l (c + rhs)
            Mul -> derive ds l (c `div` rhs)
            Div -> derive ds l (c * rhs)
    R ->
      let lhs = eval l
       in case op of
            Eq -> derive ds r lhs
            Add -> derive ds r (c - lhs)
            Sub -> derive ds r (lhs - c)
            Mul -> derive ds r (c `div` lhs)
            Div -> derive ds r (lhs `div` c)

part1 = eval (buildTree "root")

part2 =
  let t@(Node s _ l r) = buildTree "root"
      (Just d) = findStart "humn" t
   in derive d (Node s Eq l r) 0
