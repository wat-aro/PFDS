{-# LANGUAGE InstanceSigs #-}

module SplayHeap where

import Heap
import Data.Maybe

data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a)
  deriving Show

instance Heap SplayHeap where
  empty :: SplayHeap a
  empty = E

  isEmpty :: SplayHeap a -> Bool
  isEmpty E = True
  isEmpty _ = False

  insert :: Ord a => a -> SplayHeap a -> SplayHeap a
  insert x t = T a x b
    where
      (a, b) = partition x t

  merge :: Ord a => SplayHeap a -> SplayHeap a -> SplayHeap a
  merge E t = t
  merge (T a x b) t = T (merge ta a) x (merge tb b)
    where
      (ta, tb) = partition x t


  findMin :: Ord a => SplayHeap a -> Maybe a
  findMin E         = Nothing
  findMin (T E x b) = Just x
  findMin (T a x b) = findMin a

  deleteMin :: Ord a => SplayHeap a -> Maybe (SplayHeap a)
  deleteMin E = Nothing
  deleteMin (T E x b) = Just b
  deleteMin (T (T E x b) y c) = Just $ T b y c
  deleteMin (T (T a x b) y c) = Just $ T (fromMaybe E (deleteMin a)) x (T b y c)

-- bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
-- bigger pivot E = E
-- bigger pivot (T a x b)
--   | x <= pivot = bigger pivot b
--   | otherwise  = case a of
--       E -> T E x b
--       T a1 y a2 ->
--         if y <= pivot then T (bigger pivot a2) x b
--         else T (bigger pivot a1) y (T a2 x b)

-- smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
-- smaller pivot E = E
-- smaller pivot (T a x b)
--   | pivot < x = smaller pivot a
--   | otherwise = case b of
--       E -> T a x E
--       T a1 y a2 ->
--         if pivot < y then T a x (smaller pivot a2)
--         else T (T a1 x b) y (smaller pivot a2)

partition :: Ord a => a -> SplayHeap a -> (SplayHeap a, SplayHeap a)
partition pivot E = (E, E)
partition pivot t@(T a x b)
  | x <= pivot = case b of
      E -> (t, E)
      T b1 y b2 ->
        if y <= pivot then
          let (small, big) = partition pivot b2
          in (T (T a x b1) y small, big)
        else
          let (small, big) = partition pivot b1
          in (T a x small, T big y b2)
  | otherwise = case a of
      E -> (E, t)
      T a1 y a2 ->
        if y <= pivot then
          let (small, big) = partition pivot a2
          in (T a1 y small, T big x b)
        else
          let (small, big) = partition pivot a1
          in (small, T big y (T a2 x b))
