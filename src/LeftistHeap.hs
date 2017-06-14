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

rank :: Ord a => LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: Ord a => a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise        = T (rank a + 1) x b a

-- 3.2
-- わからん

-- 3.3
fromList :: Ord a => [a] -> LeftistHeap a
fromList []  = E
fromList list = fromList' $ fmap (\x -> T 1 x E E) list

fromList' :: Ord a => [LeftistHeap a] -> LeftistHeap a
fromList' []        = E
fromList' [x]       = x
fromList' (x:y:xss) = fromList' (xss ++ [merge x y])

toList :: Ord a => LeftistHeap a -> [a]
toList E = []
toList (T _ x E E) = [x]
toList (T _ x a b) = x : toList (merge a b)

-- 3.4
-- わからん
