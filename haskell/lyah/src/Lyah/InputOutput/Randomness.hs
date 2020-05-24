module Lyah.InputOutput.Randomness where

import System.Random

-- File Reading
--
-- Lots of ways to read files. Not very interesting.


-- Strings, ByteStrings, and Laziness
--
-- ByteStrings are faster than processing strings because they are not
-- as lazy. Less "thunks" (chains of executions of lazy evaluations).
-- ByteStrings are like lists, but each element is a single byte.
-- Strict ByteStrings are always evaluated as a whole. Very fast,
-- but use up memory faster than lazy structures. Lazy ByteStrings not
-- as fast as strict, but use up less memory and still are faster than
-- strings because they are stored in "chunks" of 8 bytes. So each "thunk"
-- occurs for "chunks" of 8 bytes as opposed to single characters.


-- Randomness
--
-- `random` takes a `RandomGen`, and returns a type that's an
-- instance of the `Random` typeclass and a new random generator
-- (source of randomness).
--
-- >>> random $ mkStdGen 22
-- (-3196725327075929325,902148713 2103410263)
--
-- `randoms` takes a random gen and returns an infinite stream
-- of random values.
--
-- >>> take 10 $ randoms $ mkStdGen 22
-- [-3196725327075929325,-8553619383891084173,7696160514044846940,5894808098184962166,5252716518888184666,7605537852409077165,-8689346411797143496,1673142064332208304,7036762764864851253,-4813545379819011437]
--
--`randomR` takes a range (inclusive), a random generator, and returns a
-- value in that range.
--
-- >>> randomR (1, 6) (mkStdGen 35935335)
-- (3,1250031057 40692)
--
-- randomRs makes a stream similar to randoms, but the values are in range.
--
-- `getStdGen` fetches the global random generator of your system and gives it back
-- in an IO action.
--
-- >>> :t getStdGen
-- getStdGen :: IO StdGen
--
-- `newStdGen` splits the current rng into two, updates the global, and returns
-- the other as the result. If you call newStdGen, you will get a new rng, and
-- getStdGen will also return a new StdGen.

