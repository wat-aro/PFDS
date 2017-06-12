{-# LANGUAGE InstanceSigs #-}

module BatchedQueue where

import Queue

data BatchedQueue a = BQ [a] [a]

instance Queue BatchedQueue where

  empty :: BatchedQueue a
  empty = BQ [] []

  isEmpty :: BatchedQueue a -> Bool
  isEmpty (BQ [] []) = True
  isEmpty _          = False

  head :: BatchedQueue a -> Maybe a
  head (BQ [] _)    = Nothing
  head (BQ (x:_) _) = Just x

  tail :: BatchedQueue a -> Maybe (BatchedQueue a)
  tail (BQ [] [])   = Nothing
  tail (BQ (x:f) r) = Just $ checkf (BQ f r)

  snoc :: BatchedQueue a -> a -> BatchedQueue a
  snoc (BQ f r) x  = checkf $ BQ f (x:r)

checkf :: BatchedQueue a -> BatchedQueue a
checkf (BQ [] r) = BQ (reverse r) []
checkf q         = q
