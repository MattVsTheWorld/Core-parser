module Parser_Utils where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                []     -> []
                (x:xs) -> [(x,xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                            []        -> []
                            [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                        [] -> []
                        [(v,out)] -> parse (f v ) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P(\_ -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                        []        -> parse q inp
                        [(v,out)] -> [(v,out)])

-- Verifies a predicate.
-- @Param   : A predicate on a character.
-- @Returns : A parser that satisfies the predicate.
sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
        if p x then return x else empty

------------------------------------
-- Parsers for single characters. --
------------------------------------
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

{- unused
upper :: Parser Char
upper = sat isUpper
-}

letter :: Parser Char
letter = sat isAlpha

-- Small variation on isAlphanum to include the '_' character.
isVarch :: Char -> Bool
isVarch c = isAlphaNum c || ( c == '_')

varch :: Parser Char
varch = sat isVarch

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)
------------------------------------

-- Parses a single string.
-- @Param   : String to parse. 
-- @Returns : Parser that returns the string itself as the return value.
--            Only succeeds if the entire string is found.
-- Recursively parse characters of a non-empty string; empty string can always be parsed.
string :: String -> Parser String
string [] = return []
string (x:xs) = char x    >>  
                string xs >>
                return (x:xs)

-- Parses a list of string; the first string found is returned.
-- @Param   : List of strings to parse. 
-- @Returns : Parser that returns the string itself as the return value.
--            Succeeds if any string in the list is found.
strings :: [String] -> Parser String
strings [] = return []
strings (x:xs) = do string x
                   <|> strings xs


keywords :: [String]
keywords = ["let","letrec","in","case","of","Pack"]

-- Determine whether a variable is using an illegal keyword.
isNotKey :: String -> Parser String
isNotKey = (\xs -> if all (/=xs) keywords 
                    then return xs 
                    else empty)

-- Parser for a variable (identifier).
-- A variable is a lowercase letter followed by 0 or more (many) alphanum.
-- NOTE: swap to lower to allow identifiers that start with uppercase letters.
ident :: Parser String 
ident = lower         >>= \x  ->
        --letter         >>= \x  ->
        many varch    >>= \xs ->
        isNotKey (x:xs) >>
        return (x:xs)
           

-- Parser for natural numbers.
nat :: Parser Int
nat = do xs <- some digit -- 1 or more digits
         return (read xs) -- read transforms it into an int

-- Parser to remove spaces comprising zero or more space/tab/newlines.
-- () is an empty (dummy) result value.
space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

-- Parser for positive and negative numbers.
int :: Parser Int
int = do _ <- char '-'
         n <- nat
         return (-n)
      <|> nat

-- Ignores all spaces before and after applying a parser for a token.
token :: Parser a -> Parser a 
token p = do _ <- space
             v <- p
             _ <- space
             return v

----------------------------------
-- Parsers that ignore spacing. --
----------------------------------
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

character :: Char -> Parser Char
character c = token (char c)
----------------------------------