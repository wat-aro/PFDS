module Queue where

class Queue q where
  empty   :: q a
  isEmpty :: q a -> Bool
  snoc    :: Ord a => q a -> a -> q a
  head    :: q a -> Maybe a
  tail    :: Ord a => q a -> Maybe (q a)
