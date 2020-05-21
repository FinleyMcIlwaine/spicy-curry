module BTree where

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)

-- Insertion
insertElem :: (Ord a) => a -> BTree a -> BTree a
insertElem e Empty = (Node e Empty Empty)
insertElem e (Node x l r) | x == e = (Node e l r)
                          | x > e  = (Node x l (insertElem e r))
                          | x < e  = (Node x (insertElem e l) r)

-- Builder function
buildTree :: (Ord a) => [a] -> BTree a
buildTree es = foldr (\e t -> insertElem e t) Empty es

-- Member predicate
hasElem :: (Ord a) => a -> BTree a -> Bool
hasElem e Empty = False
hasElem e (Node x l r) | e == x = True
                       | e > x  = hasElem e r
                       | e < x  = hasElem e l

-- Mapping a tree
instance Functor BTree where
  fmap f Empty        = Empty
  fmap f (Node x l r) = (Node (f x) (fmap f l) (fmap f r))

-- Scale the values of a tree
scale fac t = fmap (\e -> fac * e) t

-- Fold a tree
instance Foldable BTree where
  foldr f i Empty        = i
  foldr f i (Node x l r) = foldr f (f x (foldr f i r)) l
  foldl f i Empty        = i
  foldl f i (Node x l r) = foldl f (f (foldl f i l) x) r

foldc :: (a -> b -> b) -> b -> BTree a -> b
foldc f i Empty        = i
foldc f i (Node x l r) = foldc f (foldc f (f x i) r) l

flattenBT :: BTree a -> [a]
flattenBT t = foldr (:) [] t

instance (Num a, Eq a) => Eq (BTree a) where
  Empty        == Empty        = True
  (Node _ _ _) == Empty        = False
  Empty        == (Node _ _ _) = False
  t1           == t2           = (foldr (+) 0 t1) == (foldr (+) 0 t2)
