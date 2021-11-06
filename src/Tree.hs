module Tree where

import Data.Foldable

-- Для реализации свертки двоичных деревьев нужно выбрать алгоритм обхода узлов дерева (см., например, http://en.wikipedia.org/wiki/Tree_traversal).
-- Сделайте двоичное дерево представителем класса типов Foldable, реализовав симметричную стратегию (in-order traversal). Реализуйте также три другие стандартные стратегии (pre-order traversal, post-order traversal и level-order traversal), сделав типы-обертки представителями класса Foldable.

data Tree a = Nil | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Tree where
  foldr _ ini Nil            = ini
  foldr f ini (Branch l c r) = foldr f (c `f` foldr f ini r) l

-- Представителем класса типов Foldable, реализовав симметричную стратегию (in-order traversal). Реализуйте также три другие стандартные стратегии (pre-order traversal, post-order traversal и level-order traversal), сделав типы-обертки представителями класса Foldable.

instance Foldable Preorder where
  foldr _ ini (PreO Nil)            = ini
  foldr f ini (PreO (Branch l c r)) = c `f` foldr f (foldr f ini r') l' where
    r' = PreO r
    l' = PreO l

instance Foldable Postorder where
  foldr _ ini (PostO Nil)            = ini
  foldr f ini (PostO (Branch l c r)) = foldr f (foldr f (c `f` ini) r') l' where
    l' = PostO l
    r' = PostO r

isNil    :: Tree a -> Bool
isNil Nil = True
isNil _   = False

isBranch :: Tree a -> Bool
isBranch  = not . isNil

getRoots :: Tree a -> [a]
getRoots Nil            = error "getRoot on Nil"
getRoots (Branch _ c _) = [c]

getSibls :: Tree a -> [Tree a]
getSibls Nil            = error "getSibl on Nil"
getSibls (Branch l _ r) = [l, r]

getLevels :: [Tree a] -> [[a]]
getLevels [] = [[]]
getLevels bs = roots : getLevels sibls where
  bs'   = filter isBranch bs
  roots = bs' >>= getRoots
  sibls = bs' >>= getSibls

instance Foldable Levelorder where
  foldr _ ini (LevelO Nil) = ini
  foldr f ini (LevelO b)   = foldr f ini . concat . getLevels $ [b]

-- Try to draw tree - show it as a tree
-- First, make Tree a Functor
instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Branch l c r) = Branch (fmap f l) (f c) (fmap f r)

-- And Applicative Functor operating as a ZipList
instance Applicative Tree where
  -- pure creates infinite tree
  pure v = Branch (pure v) v (pure v)
  -- But <*> will cut infinite trees if left or right is finite
  Nil             <*> _               = Nil
  _               <*> Nil             = Nil
  Branch fl fc fr <*> Branch vl vc vr = Branch (fl <*> vl) (fc vc) (fr <*> vr)

data TreeSymbols = TS {
  lTerm :: Char, rTerm :: Char,
  bTerm :: Char,
  hBar  :: Char, vBar  :: Char,
  lCor  :: Char, rCor  :: Char
}
instance Read TreeSymbols where
  readsPrec _ [lTerm, rTerm, bTerm, hBar, vBar, lCor, rCor] =
    [ (TS lTerm rTerm bTerm hBar vBar lCor rCor, "") ]
  readsPrec _ _ = [] 

instance Show TreeSymbols where
  show (TS lTerm rTerm bTerm hBar vBar lCor rCor) =
    [lTerm, rTerm, bTerm, hBar, vBar, lCor, rCor ]

-- Symbols for tree formatting
utfSym :: TreeSymbols
-- utfSym = read "╢╟╧─│┌┐"
utfSym = read "╢╟│─│┌┐"

-- Test trees
--       ┌───╢ 3 ╟───┐
--       │           │
--       1 ╟─┐       4
--           │
--           2
tree1 :: Tree Int
tree1 = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
tree2 :: Tree [Char]
tree2 = Branch (Branch Nil "Hello" (Branch Nil "World" Nil)) "I'm" (Branch Nil "here" Nil)

maxLeafLength :: Show a => Tree a -> Int
maxLeafLength t = maximum $ length . show <$> t

leafLength :: Show a => Tree a -> Int
leafLength t =
  let mll = maxLeafLength t
  in  if odd mll then mll + 2 else mll + 3

nSpaces :: Int -> [Char]
nSpaces = flip replicate ' '

formatLeaf :: Int -> String -> String
formatLeaf n s =
  let l    = length s      -- length of the input string
      add  = max (n - l) 0 -- how many spaces should we add from left andd right
      addL = div add 2
      addR = div add 2 + mod add 2
  in nSpaces addL ++ s ++ nSpaces addR

instance Functor Preorder where
  fmap f (PreO t) = PreO (f <$> t) 

-- Prog 2.2.3
-- Предположим для двоичного дерева Tree реализован представитель класса типов Foldable, обеспечивающий стратегию обхода pre-order traversal. Какую строку вернет следующий вызов

  -- GHCi> tree = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))
  -- GHCi> fst $ sequenceA_ $ (\x -> (show x,x)) <$> tree

tree :: Preorder Integer
tree = PreO $ Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))
res223 :: String
res223 = fst $ sequenceA_ $ (\x -> (show x,x)) <$> tree
