{-# LANGUAGE InstanceSigs #-}

module PairingHeap where

import Heap

data PairingHeap a = E
                   | T a [PairingHeap a]

instance Heap PairingHeap where
  empty :: PairingHeap a
  empty = E

  isEmpty :: PairingHeap a -> Bool
  isEmpty E = True
  isEmpty _ = False

  findMin :: Ord a => PairingHeap a -> Maybe a
  findMin E = Nothing
  findMin (T x _) = Just x

  merge :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
  merge h E = h
  merge E h = h
  merge h1@(T x hs1) h2@(T y hs2)
    | x < y = T x $ h2:hs1
    | otherwise = T y $ h1:hs2

  insert :: Ord a => a -> PairingHeap a -> PairingHeap a
  insert x = merge (T x [])

  deleteMin :: Ord a => PairingHeap a -> Maybe (PairingHeap a)
  deleteMin E = Nothing
  deleteMin (T _ hs) = Just $ mergePair hs

mergePair :: Ord a => [PairingHeap a] -> PairingHeap a
mergePair [] = E
mergePair [h] = h
mergePair (h1 : h2 : hs) = merge (merge h1 h2) $ mergePair hs
