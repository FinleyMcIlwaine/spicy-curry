module Lyah.ProblemSolving.RPN where

-- | A reverse polish notation calculator!
-- >>> solveRPN "10 10 +"
-- 20.0
--
-- >>> solveRPN "46 9 /"
-- 5.111111
--
-- >>> solveRPN "10 10 10 sum 6 /"
-- 5.0
--
-- >>> solveRPN "34"
-- 34.0
--
solveRPN :: String -> Float
solveRPN = head . foldl calc [] . words
    where calc (x:y:zs) "*" = x*y:zs
          calc (x:y:zs) "+" = x+y:zs
          calc (x:y:zs) "-" = y-x:zs
          calc (x:y:zs) "/" = (y/x):zs
          calc (x:y:zs) "^" = (y**x):zs
          calc (x:xs) "ln"  = (log x):xs
          calc xs "sum"     = [sum xs]
          calc xs numStr = (read numStr):xs