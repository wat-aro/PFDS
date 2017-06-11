module BinomialHeap where
  import Heap
  import Data.Maybe

  data Tree a = Node Int a [Tree a]
    deriving Show

  newtype BinomialHeap a = BH [Tree a]

  link :: Ord a => Tree a -> Tree a -> Tree a
  link t1@(Node r x1 c1) t2@(Node _ x2 c2)
    | x1 < x2   = Node (r+1) x1 (t2:c1)
    | otherwise = Node (r+1) x2 (t1:c2)

  root :: Ord a => Tree a -> a
  root (Node _ x _) = x

  rank :: Tree a -> Int
  rank (Node r _ _) = r

  insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
  insTree t [] = [t]
  insTree t ts@(t':ts')
    | rank t < rank t' = t:ts
    | otherwise        = insTree (link t t') ts'

  removeMinTree :: Ord a => [Tree a] -> Maybe (Tree a, [Tree a])
  removeMinTree [] = Nothing
  removeMinTree [t] = Just(t, [])
  removeMinTree (t:ts)
    | root t < root t' = Just(t, ts)
    | otherwise        = Just(t', t:ts')
    where
    (t', ts') = fromJust $ removeMinTree ts

  mrg :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
  mrg [] []  = []
  mrg ts1 [] = ts1
  mrg [] ts2 = ts2
  mrg ts1@(t1 : ts1') ts2@(t2 : ts2')
    | rank t1 < rank t2 = t1 : mrg ts1' ts2
    | rank t1 > rank t2 = t2 : mrg ts1 ts2'
    | otherwise         = insTree (link t1 t2) $ mrg ts1' ts2'

  instance Heap BinomialHeap where
    empty                   = BH []
    isEmpty (BH ts)         = null ts
    insert x (BH ts)        = BH(insTree (Node 0 x []) ts)
    merge (BH ts1) (BH ts2) = BH(mrg ts1 ts2)
    findMin (BH ts)         = fmap (\(t, _) -> root t) (removeMinTree ts)
    deleteMin (BH ts)       = fmap (\(Node _ x ts1, ts2) -> BH(mrg (reverse ts1) ts2)) (removeMinTree ts)

-- 3.5
-- findMin :: BinomialHeap a -> Maybe a
-- findMin (BH []) = Nothing
--     findMin (BH ((Node _ x _):bs)) = Just x
--     deleteMin (BH ts)       = fmap (\(Node _ x ts1, ts2) -> BH(mrg (reverse ts1) ts2)) (removeMinTree ts)
