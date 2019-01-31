{-# OPTIONS_GHC -Wall #-}
module Parser where

import Expr
import Parser_Utils

import Control.Applicative
--import Data.Char


{-
Notes:
  -- Dangling else issue
  -- IMP!!! - la cosa del "-3" =/= "- 3"
  -- MINOR
  -- ignore comments (1.9)
  -- numbering ?
-}


parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do _  <- character ';'
                  ps <- parseProg
                  return (p:ps)
                 <|> 
                  return [p]

-- Supercombinator = function definition
parseScDef :: Parser (ScDef Name)
parseScDef = do v  <- identifier      -- = parseVar
                pf <- many identifier 
                _  <- character '='         -- throw away
                body <- parseExpr 
                return (v, pf, body)


-- Def -> var = expr
parseDef :: Parser (Def Name)
parseDef = do v <- identifier
              _ <- character '='
              body <- parseExpr
              do _ <- character ';' 
                 return (v, body) 
                <|> return (v, body) -- Def Name is a tuple

-- EConstr Int Int
parseConstr :: Parser (Expr Name)
parseConstr = do _     <- symbol "Pack{"
                 tag   <- natural
                 _     <- character ','
                 arity <- natural
                 _     <- character '}'
                 return (EConstr tag arity)

-- <num> var1_n -> expr
-- Alter a = (Int, [a], Expr a)
parseAlt :: Parser (Alter Name)
parseAlt = do _  <- character '<' 
              n  <- integer
              _  <- character '>'
              vs <- many identifier
              _  <- symbol "->"
              e  <- parseExpr
              do _ <- character ';'
                 return (n,vs,e)
                <|> return (n,vs,e)

-- AExpr -> var | num | Pack{num,num} | (expr)
parseAExpr :: Parser (Expr Name)
parseAExpr = do v <- identifier
                return (EVar v)
               <|>
             do n <- integer -- could expand to fractals
                return (ENum n)
               <|>
             do c <- parseConstr
                return c
               <|>
             do _ <- character '('
                e <- parseExpr
                _ <- character ')'
                return e
             -- ( expr ) 


-- case expr of alts
-- ECase (Expr a) [Alter a]                  
parseExpr :: Parser (Expr Name)
            -- Scrivere funzioni separate
parseExpr = do _ <- symbol "let"
               do _     <- symbol "rec"
                  defns <- some parseDef
                  _     <- symbol "in"
                  body  <- parseExpr
                  return (ELet Recursive defns body)
                 <|>
                  do defns <- some parseDef
                     _    <- symbol "in"
                     body <- parseExpr
                     return (ELet NonRecursive defns body)
              <|>     
            do _    <- symbol "case"
               e    <- parseExpr
               _    <- symbol "of"
               alts <- some parseAlt
               return (ECase e alts)
              <|>
            do _  <- character '\\'
               vs <- some identifier
               _  <- character '.'
               e <- parseExpr
               return (ELam vs e)
              <|>
              parseExpr1

-- expr1 -> expr2 || expr1 | expr2
-- OR 
-- associativity = right
parseExpr1 :: Parser (Expr Name) 
parseExpr1 = do e2 <- parseExpr2 
                do c  <- symbol "|"
                   e1 <- parseExpr1
                   return (EAp (EAp (EVar c) (e2)) (e1))
                  <|> 
                   return e2 
                
-- expr2 -> expr3 & expr2 | expr3
-- AND
-- associativity = right
parseExpr2 :: Parser (Expr Name)
parseExpr2 = do e3 <- parseExpr3
                do _  <- character '&'
                   e2 <- parseExpr2
                   return (EAp (EAp (EVar "&") (e3)) (e2))
                  <|> 
                   return e3
                   

-- expr3 -> expr4 relop expr4 | expr 4
-- associativity = none
-- RELOP
-- relop = [< | <= | == | ~= | >= | >]
relop :: [String]
relop = ["<","<=","==","~=",">=",">"]
 
{-
-- Unsatisfactory solution...
findRelop :: Parser String
findRelop = strings relop
   (symbol "> " >>= \xs -> return xs) 
            <|>
            (symbol "< " >>= \xs -> return xs)
            <|>
            (symbol "<=" >>= \xs -> return xs)
            <|>
            (symbol ">=" >>= \xs -> return xs)
            <|>
            (symbol "==" >>= \xs -> return xs)
            <|>
            (symbol "~=" >>= \xs -> return xs)  
            -}         
            {-
             do xs <- symbol ">"
               return xs
              <|>
            do xs <- symbol "<"
               return xs
   
            -}
       


-- Bad solution!
parseExpr3 :: Parser (Expr Name)
parseExpr3 = do e4 <- parseExpr4
                do rel <- strings relop
                   er  <- parseExpr4
                   return (EAp (EAp (EVar rel) e4) er)
                  <|>
                   return e4
              {-
             (parseExpr4   >>= \el  ->
             symbol ">"    >>= \rel ->
             parseExpr4    >>= \er  ->
             return (EAp (EAp (EVar rel) (el)) (er)))  
              -}
            {-
             do el <- parseExpr4
                rel <- symbol "<" -- || symbol ">"
                er <- parseExpr4
                --return (EAp (EVar rel) (EAp (el) (er))) -- am I drunk??
                return (EAp (EAp (EVar rel) (el)) (er))
               <|> 
                parseExpr4 
                -}
                


-- expr4 -> expr5 + exp4 | expr5 - expr5 | expr5
-- associativity = right (+), none (-)
-- right associativity == 2+3+4 = 2+(3+4) (parenthesis omission)
-- NO associativity == 3-4-3 ILLEGAL
-- BIG WARNING e.g. "5-3-2" ##
-- something with number parsing and spacing

parseExpr4 :: Parser (Expr Name)
parseExpr4 = do e5 <- parseExpr5
                do _  <- character '+'
                   e4 <- parseExpr4
                   --return (EAp (EVar ('(':c:")")) (EAp (e5) (e4)))
                   return (EAp (EAp (EVar "+") (e5)) (e4))
                  <|>
                   do _  <- character '-'
                      er <- parseExpr5
                      return (EAp (EAp (EVar "-") (e5)) (er)) -- issue
                  <|>
                   return e5

     
-- expr5 -> expr6 * expr5 | expr6 / expr6 | expr6
-- MULTIPLICATION, DIVISION
-- associativity = right, none
parseExpr5 :: Parser (Expr Name)
parseExpr5 = do e6 <- parseExpr6
                do _  <- character '*'
                   e5 <- parseExpr5
                   return (EAp (EAp (EVar "*") (e6)) (e5))
                  <|>
                   do _  <- character '/'
                      er <- parseExpr6
                      return (EAp (EAp (EVar "/") (e6)) (er))
                  <|>
                   return e6          
-- expr6 -> aexpr_1... aepxr_n (n>=1)
-- associativity = left
-- double concat f = (double concat) f

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do es <- some parseAExpr               
                return (ap_chain es)
                where 
                  -- as we have a list
                  ap_chain :: [(Expr Name)] -> (Expr Name)
                  ap_chain xs = foldl1 EAp xs --foldl1?
                  --ap_chain []     = foldl []
                  
-- TEST
test_prog :: String
test_prog = "f = 3; g x y = let z = x in z; h x = case (let y = x in y) of  <1> -> 2; <2> -> 5"
test_prog2 :: String
test_prog2 = "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2;"
-- Dangling else
{- http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/2-C/dangling-else.html


f x y = case x of
        1 -> case y of
             1 -> 1
        2 -> 2

        -}
