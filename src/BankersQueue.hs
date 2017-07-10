{-# LANGUAGE InstanceSigs #-}

module BankersQueue where

import Queue

data BankersQueue a = BQ Int [a] Int [a] deriving Show

instance Queue BankersQueue where

  empty :: BankersQueue a
  empty = BQ 0 [] 0 []

  isEmpty :: BankersQueue a -> Bool
  isEmpty (BQ lenf _ _ _) = lenf == 0

  snoc :: Ord a => BankersQueue a -> a -> BankersQueue a
  snoc (BQ lenf f lenr r) x = check (BQ lenf f (lenr + 1) (x : r))

  head :: BankersQueue a -> Maybe a
  head (BQ _ [] _ _) = Nothing
  head (BQ _ (x : f') _ _) = Just x

  tail :: Ord a => BankersQueue a -> Maybe (BankersQueue a)
  tail (BQ _ [] _ _) = Nothing
  tail (BQ lenf (x : f') lenr r) = Just $ check $ BQ (lenf - 1) f' lenr r

check :: Ord a => BankersQueue a -> BankersQueue a
check q@(BQ lenf f lenr r)
  | lenr <= lenf = q
  | otherwise    = BQ (lenf + lenr) (f ++ reverse r) 0 []
