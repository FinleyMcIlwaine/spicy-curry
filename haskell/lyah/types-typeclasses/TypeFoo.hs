module TypeFoo where

-- | Random weird typeclass
-- >>> :k Tofu
-- Tofu :: (* -> (* -> *) -> *) -> Constraint
--
class Tofu t where
    tofu :: j a -> t a j

-- | Consider:
-- >>> :t Frank {frankField = Just "HEHE"}
-- >>> :t Frank {frankField = Just 23}
-- >>> :k Frank
-- Frank {frankField = Just "HEHE"} :: Frank [Char] Maybe
-- Frank {frankField = Just 23} :: Num a => Frank a Maybe
-- Frank :: * -> (* -> *) -> *
--
data Frank a b = Frank {frankField :: b a} deriving (Show)

-- `b` must be a type constructor (* -> *), since the type of 
-- frankField must be a concrete type. This leads us to conclude
-- That the kind of Frank is (* -> (* -> *) -> *)

-- | Making Frank an instance of Tofu is pretty simple. Since
-- tofu takes a `j a` (e.g. Maybe Int) and returns a `t a j`,
-- if we replace `t` with Frank, we see `Frank a j` e.g.
-- `Frank Int Maybe`
--
-- >>> tofu (Just 'a') :: Frank Char Maybe
-- >>> tofu ["Hey there"] :: Frank [Char] []
-- Frank {frankField = Just 'a'}
-- Frank {frankField = ["Hey there"]}
--
instance Tofu Frank where
    tofu x = Frank x
