-- Higher order functions exercises 
-- From LYAH
-- Finley McIlwaine

module Lyah.Hof.Hof where

-- | Returns 4, or the number given if it is greater than 4
fourOrBigger :: (Ord a, Num a) => a -> a
fourOrBigger x | x > 4     = x
               | otherwise = 4

-- | Just a wrapper function for dividing by ten lol
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

-- | Applies a given function to the given argument, TWICE! 
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- | Zips two lists together, applying the function to corresponding
-- elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _        []       = []
zipWith' _ []       _        = []
zipWith' f (x : xs) (y : ys) = (f x y) : zipWith' f xs ys

-- | Chain gets the Collatz sequences starting at the given number
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n | even n = n : chain (n `div` 2)
        | odd n  = n : chain (n * 3 + 1)

-- Now we can make a function that gets the number
-- of chains longer than some given length for some list of
-- starting numbers
numLongChains :: (Integral a) => a -> [a] -> a
numLongChains l xs = fromIntegral (length (filter isLong (map chain xs)))
  where isLong xs =  (length xs) > (fromIntegral l)
