module Lyah.Types.TypeFoo where

-- | Random weird typeclass
class Tofu t where
    tofu :: j a -> t a j

-- We can figure out what kind `t` must be by
-- examining the functions declared in Tofu.
-- In `tofu`, we know `j` must be (* -> *), so
-- `t` is (* -> (* -> *) -> *)
-- >>> :k Tofu
-- Tofu :: (* -> (* -> *) -> *) -> Constraint
--         ^^^^^^^^^^^^^^^^^^^^
--              This is `t`

-- Let's make a type that has this kind
data Frank a b = Frank {frankField :: b a} deriving (Show)

-- `b` must be a type constructor (* -> *), since the type of 
-- frankField must be a concrete type. This leads us to conclude
-- That the kind of Frank is (* -> (* -> *) -> *)
-- >>> :k Frank
-- Frank :: * -> (* -> *) -> *
--

-- | Consider:
-- >>> :t Frank {frankField = Just "HEHE"}
-- >>> :t Frank {frankField = Just 23}
-- Frank {frankField = Just "HEHE"} :: Frank [Char] Maybe
-- Frank {frankField = Just 23} :: Num a => Frank a Maybe
--

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


-- Another example
data Barry t k p = Barry { yabba :: p, dabba :: t k }

-- We want to make this an instance of Functor, which
-- wants a kind (* -> *). But Barry's kind is
-- (* -> *) -> * -> * -> *
-- >>> :k Barry
-- Barry :: (* -> *) -> * -> * -> *
--
-- So how do we make it a Functor? Partial application of
-- the `Barry` type constructor!

instance Functor (Barry a b) where
    fmap f (Barry { yabba = x, dabba = y }) = Barry { yabba = f x, dabba = y }

-- Notice how we only map Barry's yabba! Because it's type is the
-- the last parameter in the Barry type constructor.