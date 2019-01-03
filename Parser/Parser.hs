module Parser where

import Expr
import Parser_Utils

import Control.Applicative
import Data.Char

{-
IMPORTANT NOTES:
  
  -- IMP - check for parenthesised expressions (SHOULD BE DONE)
  -- IMP? - negate function (pg 17) (?)
  -- Dangling else issue?
  -- IMP!!! - la cosa del "-3" =/= "- 3"
  -- "> " vs ">"
  
  -- MEDIUM
  -- Many some; performance problems?

  -- MINOR
  -- ignore comments (1.9)
  -- numbering ?
SOLVDED:
- Only works with integer numbers so far (ok)
- atomic expression treatment of (expr) needs checking
  -- In particular, things like "True" ? how to handle?
      -- Should be Pack{1,0}. OR modify grammar.
-- identifier should discriminate with keywords (ok)
-}

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ';'
                  ps <- parseProg
                  return (p:ps)
                 <|> 
                  return [p]
{-
Supercombinator = definizione di funzione
banana   x   xs     =       x + xs
  ^      ^   ^      ^       ^^^^^^
 Var    var var   char       expr
 Name      [a]               Expr a   
-}
parseScDef :: Parser (ScDef Name)
parseScDef = do v  <- identifier      -- = parseVar
                pf <- many identifier -- 
                character '='       -- throw away
                body <- parseExpr   -- DEFINE
                return (v, pf, body)
-- Def -> var = expr
-- Needs a checkup
parseDef :: Parser (Def Name)
parseDef = do v <- identifier
              character '='
              body <- parseExpr
              return (v, body) -- Def Name is a tuple

-- EConstr Int Int
parseConstr :: Parser (Expr Name)
parseConstr = do symbol "Pack"
                 character '{'
                 tag <- natural
                 character ','
                 arity <- natural
                 character '}'
                 return (EConstr tag arity)

-- <num> var1_n -> expr
-- Alter a = (Int, [a], Expr a)
parseAlt :: Parser (Alter Name)
parseAlt = do character '<' 
              n <- integer
              character '>'
              vs <- many identifier
              symbol "->"
              e <- parseExpr
              return (n,vs,e)

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
             do character '('
                e <- parseExpr
                character ')'
                return e
             -- ( expr ) 


{- old letrec  
-- letrec
do symbol "letrec" -- BAD SOLUTION ?
  defns <- some parseDef
  symbol "in"
  body <- parseExpr
  return (ELet Recursive defns body)
<|>
-}
-- case expr of alts
-- ECase (Expr a) [Alter a]                  
parseExpr :: Parser (Expr Name)
            -- function application
            -- needs precedence
parseExpr = do symbol "let"
               do symbol "rec"
                  defns <- some parseDef
                  symbol "in"
                  body <- parseExpr
                  return (ELet Recursive defns body)
                 <|>
                  do defns <- some parseDef
                     symbol "in"
                     body <- parseExpr
                     return (ELet NonRecursive defns body)
              <|>     
            do symbol "case"
               e <- parseExpr
               symbol "of"
               alts <- some parseAlt
               return (ECase e alts)
              <|>
              -- lambda
              -- ELam [a] (Expr a)
              -- \ var1_n . expr
              -- \ ((spazio)) vars . ((== ->)) espressione
            do character '\\'
               vs <- some identifier
               character '.'
               e <- parseExpr
               return (ELam vs e)
              <|>
              parseExpr1

-- expr1 -> expr2 || expr1 | expr2
-- OR 
-- associativity = right
{-
parseExpr1 :: Parser (Expr Name) 
parseExpr1 = do e2 <- parseExpr2 
                character '|'
                e1 <- parseExpr1
                return (EAp (EAp (EVar "|") (e2)) (e1))
               <|> 
                parseExpr2 -- ## laborious reparsing! Fixed!
-}

parseExpr1 :: Parser (Expr Name) 
parseExpr1 = do e2 <- parseExpr2 
                do c  <- character '|'
                   e1 <- parseExpr1
                   return (EAp (EAp (EVar "|") (e2)) (e1))
                  <|> 
                   return e2 
                
-- expr2 -> expr3 & expr2 | expr3
-- AND
-- associativity = right
parseExpr2 :: Parser (Expr Name)
parseExpr2 = do e3 <- parseExpr3
                do character '&'
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
 
{- M E H -}


findRelop :: Parser String
findRelop = (symbol "> " >>= \xs -> return xs) 
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
            
            {-
             do xs <- symbol ">"
               return xs
              <|>
            do xs <- symbol "<"
               return xs
   
            -}
       


-- TERRIBLE solution!
parseExpr3 :: Parser (Expr Name)
parseExpr3 = do e4 <- parseExpr4
                do rel <- findRelop
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
{-
parseExpr4 :: Parser (Expr Name)
parseExpr4 = do e5 <- parseExpr5
                character '+'
                e4 <- parseExpr4
                --return (EAp (EVar ('(':c:")")) (EAp (e5) (e4)))
                return (EAp (EAp (EVar "+") (e5)) (e4))
               <|>
             do el <- parseExpr5
                do character '-'
                   er <- parseExpr5
                   return (EAp (EAp (EVar "-") (el)) (er)) --Needs changing##
                  <|>
                   return el
-}

parseExpr4 :: Parser (Expr Name)
parseExpr4 = do e5 <- parseExpr5
                do character '+'
                   e4 <- parseExpr4
                   --return (EAp (EVar ('(':c:")")) (EAp (e5) (e4)))
                   return (EAp (EAp (EVar "+") (e5)) (e4))
                  <|>
                   do character '-'
                      er <- parseExpr5
                      return (EAp (EAp (EVar "-") (e5)) (er)) --Needs changing##
                  <|>
                   return e5

     
-- expr5 -> expr6 * expr5 | expr6 / expr6 | expr6
-- MULTIPLICATION, DIVISION
-- associativity = right, none

-- :THINKING:
parseExpr5 :: Parser (Expr Name)
parseExpr5 = do e6 <- parseExpr6
                do character '*'
                   e5 <- parseExpr5
                   return (EAp (EAp (EVar "*") (e6)) (e5))
                  <|>
                   do character '/'
                      er <- parseExpr6
                      return (EAp (EAp (EVar "/") (e6)) (er))
                  <|>
                   return e6          
-- expr6 -> aexpr_1... aepxr_n (n>=1)
-- associativity = left
-- double concat banana = (double concat) banana

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do es <- some parseAExpr               
                return (ap_chain es)
                where 
                  -- as we have a list
                  ap_chain :: [(Expr Name)] -> (Expr Name)
                  ap_chain (x:xs) = foldl EAp x xs
                  
-- TEST
test_prog = "f = 3; g x y = let z = x in z; h x = case (let y = x in y) of  <1> -> 2 <2> -> 5"
test_prog2 = "f x y = case x of <1> -> case y of <1> -> 1 <2> -> 2"
-- Dangling else
{- http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/2-C/dangling-else.html
-}
f x y = case x of
        1 -> case y of
             1 -> 1
        2 -> 2

{- to define 
parseVar :: Parser ( ?? )         DONE
parseExpr :: Parser (Expr Name)   IN THE MAKING
parseAExpr :: Parser (Expr Name)  KINDA DONE
parseDef :: Parser (Def Name)     DONE
parseAlt :: Parser (Alter Name)   DONE?
-}