module FiniteMap (FiniteMap) where

import Data.Maybe
import Prelude hiding (lookup)

-- 2.6
data FiniteMap a b = Empty
                   | Node a b (FiniteMap a b) (FiniteMap a b)
  deriving Show

bind :: (Ord a, Eq b) => a -> b -> FiniteMap a b -> FiniteMap a b
bind key value Empty = Node key value Empty Empty
bind key value finiteMap = fromMaybe finiteMap (bind' Nothing finiteMap)
  where
    bind' c Empty
      | maybe True (== key) c = Just (Node key value Empty Empty)
      | otherwise              = Nothing
    bind' c (Node fKey fValue left right)
      | key < fKey = fmap (\new -> Node fKey fValue new right) $ bind' c left
      | otherwise  = fmap (\new -> Node fKey fValue left new) $ bind' c right

lookup :: (Ord a, Eq a) => a -> FiniteMap a b -> Maybe b
lookup key finiteMap = lookup' Nothing finiteMap
  where
    lookup' c Empty = larger key c
    lookup' c (Node fKey fValue left right)
      | key < fKey = lookup' c left
      | otherwise  = lookup' (Just (fKey, fValue)) right
    larger x Nothing = Nothing
    larger x (Just (fKey, fValue))
      | fKey >= x = Just fValue
      | otherwise = Nothing
