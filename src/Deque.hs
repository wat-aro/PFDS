module Deque where

data Deque a = D [a] [a]

empty :: Deque a
empty = D [] []

isEmpty :: Deque a -> Bool
isEmpty (D [] []) = True
isEmpty _         = False

head :: Deque a -> Maybe a
head (D [] _)    = Nothing
head (D (x:_) _) = Just x

tail :: Deque a -> Maybe (Deque a)
tail (D [] _)    = Nothing
tail (D (_:f) r) = Just $ check $ D f r

last :: Deque a -> Maybe a
last (D _ [])    = Nothing
last (D _ (x:_)) = Just x

init :: Deque a -> Maybe (Deque a)
init (D _ [])    = Nothing
init (D f (_:r)) = Just $ check $ D f r

check :: Deque a -> Deque a
check (D [] r) = D (reverse secondHalf) firstHalf
  where
    (firstHalf, secondHalf) = splitAt (length r `div` 2) r
check (D f []) = D firstHalf (reverse secondHalf)
  where
    (firstHalf, secondHalf) = splitAt (length f `div` 2) f
check bq = bq
