module Lyah.Recursion.Recurse where

-- Maximum recursion!
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list? Huh?"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Maximum! But with max!
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Nope."
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

-- Replicate!
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n e
    | n <= 0 = []
    | otherwise = e : (replicate' (n-1) e)

-- Take!
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n _
    | n <= 0 = []
take' n (x:xs) = x : (take' (n-1) xs)

-- Whoa reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- Whoa! Infinite repetition!
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- Zipper
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

-- Is elem?
elem' :: (Eq a) => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs)
    | e == x = True
    | otherwise = elem' e xs

-- Quicksort!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ i | i <- xs, i <= x ]
        biggerSorted  = quicksort [ j | j <- xs, j > x ]
    in smallerSorted ++ [x] ++ biggerSorted
