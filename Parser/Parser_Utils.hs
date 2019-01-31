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
    empty = P(\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                        []        -> parse q inp
                        [(v,out)] -> [(v,out)])

-- verifies a predicate, return a Parser Char
sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
        if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower
{-
upper :: Parser Char
upper = sat isUpper
-}
letter :: Parser Char
letter = sat isAlpha

-- small variation on isAlphanum to include the '_' character
isVarch :: Char -> Bool
isVarch c = isAlphaNum c || ( c == '_')

varch :: Parser Char
varch = sat isVarch
-------------------------

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

-- Empty string can always be parsed
-- recursively parse characters of a non-empty string
string :: String -> Parser String
string [] = return []
string (x:xs) = -- char x >>= string xs >>= return (x:xs)
                char x    >> --  x >> y = x >>= \_ -> y  
                string xs >>
                return (x:xs)
                
                {-do char x
                   string xs
                   return (x:xs)
                -}
strings :: [String] -> Parser String
strings [] = return []
strings (x:xs) = do string x
                   <|> strings xs

-- Parser for identifier (var name)
-- lowercase letter followed by 0 or more (many) alphanum
-- Added _ as a legal character for identifiers
keywords :: [String]
keywords = ["let","letrec","in","case","of","Pack"]

isNotKey :: String -> Parser String
isNotKey xs = if all (/=xs) keywords 
           then return xs 
           else empty

-- can put lower to enforce variables to start with a lowercase character
ident :: Parser String 
ident = lower         >>= \x  ->
        --letter         >>= \x  ->
        many varch    >>= \xs ->
        isNotKey (x:xs) >>
        return (x:xs)
        {- do x  <- lower -- debatable point
           xs <- many varch
           return (x:xs)
        -}
           -- elem ([x] ++ xs) keywords
           -- return []
           

-- parser for natural numbers
nat :: Parser Int
nat = do xs <- some digit -- 1 or more digits
         return (read xs) -- read per trasformarlo in Int

-- UNUSED : fractionals
{-
fract :: Parser Double
fract = do x <- some digit
           character '.'
           xs <- some digit
           return (read (x ++ "." ++ xs))

fractional :: Parser Double
fractional = token fract
-- /UNUSED : fractionals
-}

-- Parser to remove spaces comprising zero or more space/tab/newlines
-- () dummy result value
space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

-- we can now do integers in general (+/-)
int :: Parser Int
int = do _ <- char '-'
         n <- nat
         return (-n)
      <|> nat

-- SPACING
-- handling spacing

-- Ignores all spaces before and after applying a parser for a token
token :: Parser a -> Parser a 
token p = do _ <- space
             v <- p
             _ <- space
             return v

-- can now define parsers that ignore spaces
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

 
-- Parse non empty list of natural numbers
{-
nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)
-}
-- Arithmetic expressions
{-
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
            <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
           <|> natural


-- evaluation of an expression ###
-- 
eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n,[])]  -> n
            [(_,out)] -> error ("Unused input " ++ out)
            []        -> error "Invalid input"
-}