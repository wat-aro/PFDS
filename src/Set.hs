module Set where

class Set s where
  empty  :: Ord a => s a
  insert :: Ord a => a -> s a -> s a
  member :: Ord a => a -> s a -> Bool
