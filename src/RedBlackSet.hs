module RedBlackSet where

import Set

data Color = R | B deriving Show
data RedBlackSet a = E
                   | T Color (RedBlackSet a) a (RedBlackSet a)

instance Set RedBlackSet where
  empty = E
  member _ E = False
  member x (T _ left y right)
    | x < y     = member x left
    | x > y     = member x right
    | otherwise = True
  insert x s = T B left y' right
    where
      ins E = T R E x E
      ins s1@(T color a y b)
        | x < y     = balance color (ins a) y b
        | x > y     = balance color a y $ ins b
        | otherwise = s1
      T _ left y' right = ins s

balance :: Ord a => Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y $ T B c z d
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y $ T B c z d
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y $ T B c z d
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y $ T B c z d
balance color a x b                = T color a x b
