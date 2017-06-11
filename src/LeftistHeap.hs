module LeftistHeap where

import Heap

data LeftistHeap a = E
            | T Int a (LeftistHeap a) (LeftistHeap a)
  deriving Show

instance Heap LeftistHeap where
  empty = E
  isEmpty E = True
  isEmpty _ = False
  insert x = merge (T 1 x E E)
  merge left E = left
  merge E left = left
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
    | x < y     = makeT x a1 $ merge b1 h2
    | otherwise = makeT y a2 $ merge h1 b2
  findMin (T _ x _ _) = Just x
  findMin _           = Nothing
  deleteMin (T _ _ a b) = Just $ merge a b
  deleteMin _ = Nothing

rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise        = T (rank a + 1) x b a
