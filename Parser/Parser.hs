{-# OPTIONS_GHC -Wall #-}
module Parser where

import Expr
import Parser_Utils
import Control.Applicative
{-
-------------------------------------------------------------------
  A parser for the core language
  @Author : Matteo Marcuzzo 1207249 
  @References: - "Implementing Functional Languages: a tutorial" 
                        by Simon L Peyton Jones and David R Lester
               - "Programming in Haskell" 
                        by Graham Hutton
-------------------------------------------------------------------
-}

-- Parses a program, composed of supercombinators (functions) separated by semicolons.
parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do _  <- character ';'
                  ps <- parseProg
                  return (p:ps)
                 <|> 
                  return [p]

-- Parses a supercombinator (function) definition.
-- Composed of 1 or more variable identifier and the defining body (expression).
parseScDef :: Parser (ScDef Name)
parseScDef = do v    <- identifier            -- parseVar
                pf   <- many identifier 
                _    <- character '='         -- discard parsed symbol
                body <- parseExpr 
                return (v, pf, body)


-- Parses a single definition, composed by a single variable identifier and the defining expression.
-- The definition may or may not end with a semicolon.
parseDef :: Parser (Def Name)
parseDef = do v    <- identifier
              _    <- character '='
              body <- parseExpr
              do _ <- character ';' 
                 return (v, body) 
                <|> return (v, body)          -- Def Name is a tuple (Name, Expr Name)

-- Parses a constructor for a structured type.
-- The core language provides a single family of constructors.
-- These are presented in the form Pack{tag, arity}.
parseConstr :: Parser (Expr Name)
parseConstr = do _     <- symbol "Pack{"
                 tag   <- natural
                 _     <- character ','
                 arity <- natural
                 _     <- character '}'
                 return (EConstr tag arity)   -- EConstr Int Int

-- Parses an alternative for a case expression.
-- each alternative consists of a <tag> followed by a number of variables,
-- followed by an expression to evaluate.
parseAlt :: Parser (Alter Name)
parseAlt = do _  <- character '<' 
              n  <- integer
              _  <- character '>'
              vs <- many identifier
              _  <- symbol "->"
              e  <- parseExpr
              return (n,vs,e)                 -- Alter Name = (Int, [Name], Expr Name)

-- Parses an atomic expression.
-- This is either a variable, a number, a constructor or a parenthesised expression.
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

-- Parses a local definition. This may be non-recursive (let) or recursive (letrec).
-- The parser doesn't check if the definition is actually recursive or not.
-- Local definition constructs are expressions and may be used as such.
-- A local definition is composed by a series of definitions and a body expression.
parseLet :: Parser (Expr Name)
parseLet = do _ <- symbol "let"
              do _     <- symbol "rec"
                 defns <- some parseDef
                 _     <- symbol "in"
                 body  <- parseExpr
                 return (ELet Recursive defns body)      -- ELet IsRec [Def Name] (Expr Name)
                 <|>
               do defns <- some parseDef
                  _     <- symbol "in"
                  body  <- parseExpr
                  return (ELet NonRecursive defns body)  -- ELet IsRec [Def Name] (Expr Name)

-- Parses a case expression. This replaces all forms of pattern matching in the core language.
-- A case expression can be seen as a multi-way jump: there is an expression to evaluate,
-- whose evaluation returns a tag (part of the constructor). The tag points to the appropriate alternative.
-- NOTE: Each alternative is generally separated by a semicolon. The last semicolon represents the end
-- of the case expression. If such expression is the very last of the program, it might not have a semicolon.
parseCase :: Parser (Expr Name)
parseCase = do _    <- symbol "case"
               e    <- parseExpr
               _    <- symbol "of"
               alt  <- parseAlt
               alts <- many (do _ <- symbol ";"
                                parseAlt)
               return (ECase e (alt:alts))    -- ECase (Expr Name) [Alter Name]

-- Parses a lambda abstraction. 
-- This is comprised by a series of argument identifiers and an expression that uses them.
parseLambda :: Parser (Expr Name)
parseLambda = do _  <- character '\\'
                 vs <- some identifier
                 _  <- character '.'
                 e  <- parseExpr
                 return (ELam vs e)           -- ELam [Name] (Expr Name)

-- Parses an expression, following an appropriate order.
-- First check if a parse may be a let, case or lambda expression;
-- Then, check for application and infix operators based on their precedence.               
parseExpr :: Parser (Expr Name)
parseExpr = parseLet
           <|>     
            parseCase
           <|>
            parseLambda
           <|>
            parseExpr1

------------------------------------------------------------------------------------------
--------------------------------    Expression parsing    --------------------------------       
------------------------------------------------------------------------------------------
-- Note that nested do operators simulate partial expressions used by the book.



-- OR boolean operator.
-- expr1 -> expr2 || expr1 | expr2
-- Associativity = Right
parseExpr1 :: Parser (Expr Name) 
parseExpr1 = do e2 <- parseExpr2 
                do _  <- character '|'
                   e1 <- parseExpr1
                   return (EAp (EAp (EVar "|") (e2)) (e1))
                  <|> 
                   return e2 

-- AND boolean operator.
-- expr2 -> expr3 & expr2 | expr3
-- Associativity = Right
parseExpr2 :: Parser (Expr Name)
parseExpr2 = do e3 <- parseExpr3
                do _  <- character '&'
                   e2 <- parseExpr2
                   return (EAp (EAp (EVar "&") (e3)) (e2))
                  <|> 
                   return e3
                   
-- RELOP comparison operators.
-- expr3 -> expr4 relop expr4 | expr 4
-- Associativity = None
relop :: [String]
relop = ["<","<=","==","~=",">=",">"]

parseExpr3 :: Parser (Expr Name)
parseExpr3 = do e4 <- parseExpr4
                do rel <- strings relop
                   er  <- parseExpr4
                   return (EAp (EAp (EVar rel) e4) er)
                  <|>
                   return e4
-- Addition and Difference operators.            
-- expr4 -> expr5 + exp4 | expr5 - expr5 | expr5
-- Associativity = Right (+), None (-)
-- right associativity means that expressions such as 2+3+4 = 2+(3+4) (parenthesis omission)
-- No associativity means that expression such as 3-4-3 are illegal
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

-- Multiplication and Division operators.    
-- expr5 -> expr6 * expr5 | expr6 / expr6 | expr6
-- Associativity = Right (*), None (/)
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

-- Function application.
-- expr6 -> aexpr_1... aepxr_n (n>=1)
-- Associativity = Left
parseExpr6 :: Parser (Expr Name)
parseExpr6 = do es <- some parseAExpr               
                return (ap_chain es)
                where 
                  -- as we have a list
                  ap_chain :: [(Expr Name)] -> (Expr Name)
                  ap_chain xs = foldl1 EAp xs 
       
{-
-- TEST
test_prog :: String
test_prog = "f = 3; g x y = let z = x in z; h x = case (let y = x in y) of  <1> -> 2; <2> -> 5; k = 15; u = k + f"
test_prog2 :: String
test_prog2 = "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2;"
-- Dangling else
{- http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/2-C/dangling-else.html
-}
-}

