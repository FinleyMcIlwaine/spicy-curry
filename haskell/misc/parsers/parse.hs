module Parser where

-- A parser is a function that accepts a string and
-- returns a list of pairs.
-- The pairs contain things of type a (the successfully
-- parsed bits of the input) and a string (the remaining
-- input). It returns a list of these pairs because of
-- ambiguity; if there is more than one way to parse the
-- input.
newtype Parser a = Parser (String -> [(a,String)])

-- A note: Failure is denoted by the empty list. While it
-- may seem that the parser "fails" if there is still
-- input remaining after a parse (as is the notion in
-- traditional bottom-up parsining), this is not the case.
-- Functional, recursive descent parsers parse one bit
-- of the input at a time, with failure occurring if one
-- bit cannot be parsed by the given parser.


-- FIRST PARSER -- the item parser
-- Just consumes and returns the first character of the
-- input. Fails if empty string
item :: Parser Char
item = Parser (\cs -> case cs of
                        ""      -> []
                        (c:cs)  -> [(c,cs)]


-- MONADIC PARSING
-- Now we make the Parser type an instance of the Monad
-- tyepclass!
instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                (a,cs') <- parse p cs])
-- return:
-- produces a parser that succeeds always everything, but
-- only returns the single value a.
--
-- (>>=):
-- sequencing operator for parsers.
