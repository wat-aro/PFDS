module Queue where

class Queue q where
  empty   :: q a
  isEmpty :: q a -> Bool
  snoc    :: q a -> a -> q a
  head    :: q a -> Maybe a
  tail    :: q a -> Maybe (q a)
