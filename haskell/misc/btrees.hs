module BTree where

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Read)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = singleton x
treeInsert x (Node a l r)
    | x == a = Node x l r
    | x < a  = Node a (treeInsert x l) r
    | x > a  = Node a l (treeInsert x r)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Node a l r)
    | x == a = True
    | x < a  = treeElem x l
    | x > a  = treeElem x r

buildTree :: (Ord a) => [a] -> Tree a
buildTree l = foldr (\x t -> treeInsert x t) Empty l


-- Enter the Functor
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- Enter foldable
instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Node a l r) = foldr f (f a (foldr f z r)) l
