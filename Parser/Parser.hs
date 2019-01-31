{-# OPTIONS_GHC -Wall #-}
module Parser where

import Expr
import Parser_Utils
import Control.Applicative

{-
One or more:
some :: f a -> f [a]

Zero or more:
many :: f a -> f [a]
-}

{-
The do notation combines parsers in sequence, with the output string 
from each parser in the sequence becoming the input string for the next.
-}

{-
  A parser for the core language
  @Author : Matteo Marcuzzo 1207249 
  @References: "Implementing Functional Languages: a tutorial" by Simon L Peyton Jones and David R Lester
               "Programming in Haskell" by Graham Hutton
-}



-- Parses a program, composed of supercombinators (functions) separated by semicolons
parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do _  <- character ';'
                  ps <- parseProg
                  return (p:ps)
                 <|> 
                  return [p]

-- Parses a supercombinator (function) definition
-- Composed of 1 or more variable identifier and the defining body (expression)
parseScDef :: Parser (ScDef Name)
parseScDef = do v    <- identifier            -- parseVar
                pf   <- many identifier 
                _    <- character '='         -- discard parsed symbol
                body <- parseExpr 
                return (v, pf, body)


-- Parses a single definition, composed by a single variable identifier and the defining expression
-- The definition may or may not end with a semicolon
parseDef :: Parser (Def Name)
parseDef = do v    <- identifier
              _    <- character '='
              body <- parseExpr
              do _ <- character ';' 
                 return (v, body) 
                <|> return (v, body)          -- Def Name is a tuple (Name, Expr Name)

-- Parses a constructor for a structured type
-- The core language provides a single family of constructors
-- These are presented in the form Pack{tag, arity}
parseConstr :: Parser (Expr Name)
parseConstr = do _     <- symbol "Pack{"
                 tag   <- natural
                 _     <- character ','
                 arity <- natural
                 _     <- character '}'
                 return (EConstr tag arity)   -- EConstr Int Int

-- Parses an alternative for a case expression
-- each alternative consists of a <tag> followed by a number of variables, 
-- followed by an expression to evaluate
parseAlt :: Parser (Alter Name)
parseAlt = do _  <- character '<' 
              n  <- integer
              _  <- character '>'
              vs <- many identifier
              _  <- symbol "->"
              e  <- parseExpr
              return (n,vs,e) -- Alter Name = (Int, [Name], Expr Name)

-- Parses an atomic expression
-- This is either a variable, a number, a constructor or a parenthesised expression
-- AExpr -> var | num | Pack{num,num} | (expr)
parseAExpr :: Parser (Expr Name)
parseAExpr = do v <- identifier
                return (EVar v)
               <|>
             do n <- integer
                return (ENum n)
               <|>
             do c <- parseConstr
                return c
               <|>
             do _ <- character '('
                e <- parseExpr
                _ <- character ')'
                return e


parseLet :: Parser (Expr Name)
parseLet = do _ <- symbol "let"
              do _     <- symbol "rec"
                 defns <- some parseDef
                 _     <- symbol "in"
                 body  <- parseExpr
                 return (ELet Recursive defns body)
                 <|>
               do defns <- some parseDef
                  _     <- symbol "in"
                  body  <- parseExpr
                  return (ELet NonRecursive defns body)

parseCase :: Parser (Expr Name)
parseCase = do _    <- symbol "case"
               e    <- parseExpr
               _    <- symbol "of"
               alt  <- parseAlt
               alts <- many (do _ <- symbol ";"
                                parseAlt)
               return (ECase e (alt:alts))

parseLambda :: Parser (Expr Name)
parseLambda = do _  <- character '\\'
                 vs <- some identifier
                 _  <- character '.'
                 e  <- parseExpr
                 return (ELam vs e)

-- case expr of alts
-- ECase (Expr a) [Alter a]                  
parseExpr :: Parser (Expr Name)
parseExpr = parseLet
           <|>     
            parseCase
           <|>
            parseLambda
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

parseExpr3 :: Parser (Expr Name)
parseExpr3 = do e4 <- parseExpr4
                do rel <- strings relop
                   er  <- parseExpr4
                   return (EAp (EAp (EVar rel) e4) er)
                  <|>
                   return e4
            

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
                  ap_chain xs = foldl1 EAp xs 
                  --ap_chain []     = foldl []
                  
-- TEST
test_prog :: String
test_prog = "f = 3; g x y = let z = x in z; h x = case (let y = x in y) of  <1> -> 2; <2> -> 5; k = 15; u = k + f"
test_prog2 :: String
test_prog2 = "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2;"
-- Dangling else
{- http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/2-C/dangling-else.html


f x y = case x of
        1 -> case y of
             1 -> 1
        2 -> 2

        -}
