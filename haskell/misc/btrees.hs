-- Ultra lame binary tree eq type class stuff

module BTree where

data BTree a = Leaf a | Node a (BTree a) (BTree a)
    deriving (Show)

instance (Eq a) => Eq (BTree a) where
    Leaf x == Leaf y = x == y
    Node x b1 b2 == Node y b3 b4 = (elementOf b3 x || elementOf b4 x) && (elementOf b1 y || elementOf b2 y) && (b1==b3 || b1 == b4) && (b2 == b3 || b2 == b4)

elementOf :: (Eq a) => (BTree a) -> a -> Bool
elementOf (Leaf x) e = True
elementOf (Node x b1 b2) e
    | x == e = True
    | otherwise = (elementOf b1 e) || (elementOf b2 e)
