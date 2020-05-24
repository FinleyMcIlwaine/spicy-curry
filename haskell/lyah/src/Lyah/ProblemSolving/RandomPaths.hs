module Lyah.ProblemSolving.RandomPaths where

import Lyah.ProblemSolving.HeathrowLondon
import System.Random

nSectionRoadSystem :: (RandomGen gen) => Int -> gen -> RoadSystem
nSectionRoadSystem 0 _ = []
nSectionRoadSystem n g =
    let
        threeRands = take 3 $ randomRs (0,100) g
    in (Section (threeRands!!0) (threeRands!!1) (threeRands!!2)) : nSectionRoadSystem (n-1) (mkStdGen (threeRands!!0))

main :: IO ()
main = do
    putStrLn "How many sections would you like in the road system?"
    n <- getLine
    gen <- getStdGen
    let system = nSectionRoadSystem (read n) gen
        path = optimalPath system
        pathStr = concat $ map (show . fst) path
        pathCost = sum $ map snd path
    putStrLn $ "Road system:\n" ++ (show system)
    putStrLn $ "The optimal path is:      " ++ pathStr
    putStrLn $ "The cost of that path is: " ++ show pathCost