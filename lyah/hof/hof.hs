module Hof where

fourOrBigger :: (Ord a, Num a) => a -> a
fourOrBigger x
    | x > 4 = x
    | otherwise = 4

divideByTen :: (Floating a) => a -> a
divideByTen = (/10) 
