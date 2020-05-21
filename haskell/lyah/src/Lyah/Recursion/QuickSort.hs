module Lyah.Recursion.QuickSort where

quicksort :: (Ord a, Eq a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = smalls ++ [x] ++ bigs
    where smalls = quicksort [s | s <- xs, s < x]
          bigs   = quicksort [b | b <- xs, b >= x]

