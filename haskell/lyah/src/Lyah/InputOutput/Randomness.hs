module Lyah.InputOutput.Randomness where

import System.Random

-- `random` takes a `RandomGen`, and returns a type that's an
-- instance of the `Random` typeclass and a new random generator
-- (source of randomness).

a :: (Int, StdGen)
a = random $ mkStdGen 22
