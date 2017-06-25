module RedBlackSet where

import Set

data Color = R | B deriving Show
data RedBlackSet a = E
                   | T Color (RedBlackSet a) a (RedBlackSet a)
  deriving Show

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
      ins s1@(T color E y E)
        | x > y = T color E y $ T R E x E
        | x < y = T color (T R E x E) y E
        | otherwise = s1
      ins s1@(T color E y b@(T _ _ by _))
        | x < y           = T color (T R E x E) y b
        | x > y && x < by = rlbalance color E y $ ins b
        | x > y && x > by = rrbalance color E y $ ins b
        | otherwise       = s1
      ins s1@(T color a@(T _ _ ay _) y E)
        | x < y && x < ay = llbalance color (ins a) y E
        | x < y && x > ay = lrbalance color (ins a) y E
        | x > y           = T color a y (T R E x E)
        | otherwise       = s1
      ins s1@(T color a@(T _ _ ay _) y b@(T _ _ by _))
        | x < y && x < ay = llbalance color (ins a) y b
        | x < y && x > ay = lrbalance color (ins a) y b
        | x > y && x < by = rlbalance color a y $ ins b
        | x > y && x > by = rrbalance color a y $ ins b
        | otherwise       = s1
      T _ left y' right = ins s

-- balance :: Ord a => Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
-- balance B (T R (T R a x b) y c) z d = T R (T B a x b) y $ T B c z d
-- balance B (T R a x (T R b y c)) z d = T R (T B a x b) y $ T B c z d
-- balance B a x (T R (T R b y c) z d) = T R (T B a x b) y $ T B c z d
-- balance B a x (T R b y (T R c z d)) = T R (T B a x b) y $ T B c z d
-- balance color a x b                = T color a x b

-- 3.9
fromOrdList :: Ord a => [a] -> RedBlackSet a
fromOrdList = foldr insert E

-- 3.10
-- a
-- lbalance :: Ord a => Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
-- lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y $ T B c z d
-- lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y $ T B c z d
-- lbalance color a x b                 = T color a x b

rbalance :: Ord a => Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y $ T B c z d
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y $ T B c z d
rbalance color a x b                 = T color a x b

-- b
llbalance :: Ord a => Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
llbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y $ T B c z d
llbalance color a x b                 = T color a x b

lrbalance :: Ord a => Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
lrbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y $ T B c z d
lrbalance color a x b                 = T color a x b

rlbalance :: Ord a => Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
rlbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y $ T B c z d
rlbalance color a x b                 = T color a x b

rrbalance :: Ord a => Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
rrbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y $ T B c z d
rrbalance color a x b                 = T color a x b
