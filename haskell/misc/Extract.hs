module Extract where

type Var = String

data Formula = Bottomt | FVar String | Formula `Implies` Formula | Formula `And` Formula
    deriving (Eq,Show)

type Sequent = ([(String, Formula)], [Formula])

data Term = TVar String | Lambda String Term | Ap Term Term | Fst Term | Snd Term
    deriving (Eq,Show)


fresh x vars =
    if x `elem` vars then
        fresh (x ++ "'") vars
    else
        x

squash [] = False
squash (x:_) = True

prove :: Sequent -> [String] -> ([Term],[String])
prove (hyps, concls) vars
    | squash getAxVars = ([axiom], vars)
    | squash getImpliesRight = ([Lambda x' (head t1)], vars)
    | otherwise = ([],vars)
        where getAxVars = filter (\(x,f) -> f `elem` concls) hyps
              axiom = TVar (fst (head getAxVars))
              getImpliesRight = filter (\f -> case f of { (Implies p q) -> True; _ -> False } )  concls
              Implies p q = head getImpliesRight
              (t1,vars') = prove ((x',p) : hyps, q : filter (\f -> f /= (Implies p q)) concls) (x' : vars)
              x' = fresh "x" vars

