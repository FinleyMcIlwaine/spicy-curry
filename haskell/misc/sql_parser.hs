-- A parser for a very simple subset of SQL
-- Finley McIlwaine

module SQLParser where

-- The parser type
newtype Parser a = MkP (String -> [(a,String)])

-- The parse function
parse :: Parser a -> String -> [(a,String)]
parse (MkP f) i = f i

-- I want some do
instance Functor Parser where
    fmap g p = MkP (\s -> case parse p s of
                          []        -> []
                          [(v,vs)]    -> [(g v,vs)])

instance Applicative Parser where
    p <*> q = MkP (\i -> case parse p i of
                         []         -> []
                         [(v,vs)]   -> parse (fmap v q) vs)
    pure v = MkP (\i -> [(v,i)])

instance Monad Parser where
    p >>= q = MkP f
        where f s = [(y,s'') | (x,s') <- parse p s, (y,s'') <- parse (q x) s']
    return v = MkP(\i -> [(v,i)])

-- The SQL abstract syntax
data TableName = TableName String | Anon [FieldName] deriving (Show,Eq)
data FieldName = FieldName String | FieldRef TableName String deriving (Eq,Show)
data Type = BOOL | INT | STRING | NULL Type deriving (Eq,Show)
data Values = BoolV Bool | IntV Int | StringV String deriving (Eq,Show)
data Tuple = T [Values] deriving (Show,Eq)
data Exp =
    C Int
    | FN FieldName
    | Or Exp Exp
    | And Exp Exp
    | Not Exp
    | Equal Exp Exp
    | LT Exp Exp
    | Plus Exp Exp
    | Times Exp Exp
    deriving (Show,Eq)

data Table =
    CreateTable TableName [(String,Type)]
    | InsertInto Table Tuple
    | JoinOn Table Table Exp
    | SelectFrom [FieldName] TableName
    | SelectFromWhere [FieldName] TableName Exp
    | Table TableName
    | Sequence Table Table
    | Go Table Table
    deriving (Eq,Show)

-- Our parsers!
-- First, because left recursion has been removed, and we need to
-- deal with "empty string" productions, we define a parser for
-- handling this
thenMaybe :: Parser a -> (a -> Parser a) -> Parser a
p `thenMaybe` q = do r1 <- p
                     r2 <- q r1
                     return r2
failure :: Parser a
failure = MkP(\i -> [])

-- Now we define some utility parsers
item :: Parser Char
item = MkP(\i -> case i of
                      []        -> []
                      (x:xs)    -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\x -> if p x then return x else failure)

digit :: Parser Char 
digit = sat (\i -> elem i ['0'..'9'])

lower :: Parser Char
lower = sat (\i -> elem i ['a'..'z'])

upper :: Parser Char
upper = sat (\i -> elem i ['A'..'Z'])

char :: Char -> Parser Char
char c = sat (\i -> i == c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = MkP (\i -> case parse p i of
                     []         -> parse q i
                     m          -> m)

