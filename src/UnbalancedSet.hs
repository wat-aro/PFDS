module UnbalancedSet (UnbalancedSet) where

import Set
import Data.Maybe

data UnbalancedSet a = Leaf
                     | Branch a (UnbalancedSet a) (UnbalancedSet a)
  deriving Show

instance Set UnbalancedSet where
  empty  = Leaf
  member x = member' Nothing
    where
      member' c Leaf = maybe False (>= x) c
      member' c (Branch y left right)
        | x < y = member' c left
        | otherwise = member' (Just y) right
  insert x unbalancedSet = fromMaybe unbalancedSet (insert' Nothing unbalancedSet)
    where
      insert' c Leaf
        | maybe True (== x) c = Just (Branch x Leaf Leaf)
        | otherwise            =  Nothing
      insert' c (Branch y left right)
        | x < y     = fmap (\new -> Branch y new right) (insert' c left)
        | otherwise = fmap (\new -> Branch y left new) (insert' (Just y) right)

-- insert :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
-- insert x Leaf = Branch x Leaf Leaf
-- insert x tree@(Branch y left right)
--   | x < y = Branch y (insert x left) right
--   | x > y = Branch y left (insert x right)
--   | otherwise = tree

-- member :: Ord a => a -> UnbalancedSet a -> Bool
-- member x Leaf = False
-- member x (Branch y left right)
--   | x < y = member x left
--   | x > y = member x right
--   | otherwise = True

-- 2.2
-- member :: Ord a => a -> UnbalancedSet a -> Bool
-- member x = member' Nothing
--   where
--     member' c Leaf = maybe False (>= x) c
--     member' c (Branch y left right)
--       | x < y = member' c left
--       | otherwise = member' (Just y) right

-- 2.3
-- insert :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
-- insert x UnbalancedSet = fromMaybe UnbalancedSet (insert' UnbalancedSet)
--   where
--     insert' Leaf = Just (Branch x Leaf Leaf)
--     insert' (Branch y left right)
--       | x < y     = fmap (\new -> Branch y new right) (insert' left)
--       | y < x     = fmap (\new -> Branch y left new) (insert' right)
--       | otherwise = Nothing

-- 2.4

-- insert :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
-- insert x unbalancedSet = fromMaybe unbalancedSet (insert' Nothing unbalancedSet)
--   where
--     insert' c Leaf
--       | maybe True (== x) c = Just (Branch x Leaf Leaf)
--       | otherwise            =  Nothing
--     insert' c (Branch y left right)
--       | x < y     = fmap (\new -> Branch y new right) (insert' c left)
--       | otherwise = fmap (\new -> Branch y left new) (insert' (Just y) right)

-- 2.5 完全二分木
complete :: Ord a => a -> Int -> UnbalancedSet a
complete _ 0 = Leaf
complete x d = Branch x subtree subtree
  where
    subtree = complete x (d - 1)

-- 平衡木
makeBinary :: Ord a => a -> Int -> UnbalancedSet a
makeBinary x n = fst $ create2 x n
  where
    create2 :: Ord a => a -> Int -> (UnbalancedSet a, UnbalancedSet a)
    create2 x 0 = (Leaf, Branch x Leaf Leaf)
    create2 x n
      | even n    = let (t1, t2) = create2 x (n `div` 2 - 1)
                    in (Branch x t1 t2, Branch x t2 t2)
      | otherwise = let (t1, t2) = create2 x (n `div` 2)
                    in (Branch x t1 t1, Branch x t1 t2)
