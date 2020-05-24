module Lyah.ProblemSolving.HeathrowLondon where

import Data.List

-- | Shortest path from Heathrow to London on road sections
--   of this structure:
--
--  Ai --- 10 --- A(i+1) ...
--                  |
--                  4
--                  |
--  Bi --- 19 --- B(i+1) ...

-- How we model the data involved
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0 ]

-- Path data
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- Solving the sub-problem of a single section
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let
        costA = sum $ map snd pathA
        costB = sum $ map snd pathB
        costForwardA = costA + a
        costCrossA   = costB + b + c
        costForwardB = costB + b
        costCrossB   = costA + a + c
        newPathA = if costForwardA <= costCrossA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        newPathB = if costForwardB <= costCrossB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
    in (newPathA,newPathB)

-- Get the optimal path for an arbitrary road system
-- >>> optimalPath heathrowToLondon
-- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
--
optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let
        (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
        aCost = sum $ map snd bestAPath
        bCost = sum $ map snd bestBPath
    in if aCost <= bCost
            then reverse bestAPath
            else reverse bestBPath

-- Now lets get a road system from stdin!

-- | `groupsOf` turns a list of elements into a list
--   of groups of elements of size `n`.
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = []
groupsOf n [] = []
groupsOf n xs = (take n xs) : groupsOf n (drop n xs)

-- Play along at home!
-- `cd` to this folder
-- cat paths.txt | runhaskell HeathrowLondon.hs
--
-- output:
-- The optimal path is:      BCACBBC
-- The cost of that path is: 75
main :: IO ()
main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathStr = concat $ map (show . fst) path
        pathCost = sum $ map snd path
    putStrLn $ "The optimal path is:      " ++ pathStr
    putStrLn $ "The cost of that path is: " ++ show pathCost
