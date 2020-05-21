module AllPairs where

all_pairs :: [a] -> [b] -> [(a,b)]
all_pairs [] _ = []
all_pairs _ [] = []
all_pairs xs ys = [(x,y) | x <- xs, y <- ys]
