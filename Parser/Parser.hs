module Parser where

import Expr
import Parser_Utils

import Control.Applicative
import Data.Char
import Data.List

{-
IMPORTANT NOTES:
- Only works with integer numbers so far
- atomic expression treatment of (expr) needs checking
  -- In particular, things like "True" ? how to handle?
      -- Should be Pack{1,0}. OR modify grammar.
  -- missing application / binops
  -- identifier should discriminate with keywords
  -- maybe there should be some choices? (see parseProg)
  -- are we using empty at all? Am I retarded?
-}

-- ESEMPI
parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ';'
                  ps <- parseProg
                  return (p:ps)
                 <|> return [p]
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
                return (c)
               <|>
             do character '('
                e <- parseExpr -- correct? mmmh
                character ')'
                return (e)
             -- ( expr ) 


parseExpr :: Parser (Expr Name)
            -- function application
            -- needs precedence
parseExpr = {- Nope
                do (e:es)  <- some parseAExpr
               return (EAp () ())
               <|>
               -}
            -- let
            -- ELet IsRec [Def a] (Expr a)
            do symbol "let" 
               defns <- some parseDef
               symbol "in"
               body <- parseExpr
               return (ELet NonRecursive defns body)
              <|>
              -- letrec
            do symbol "letrec" -- BAD SOLUTION
               defns <- some parseDef
               symbol "in"
               body <- parseExpr
               return (ELet Recursive defns body)
              <|>
              -- case expr of alts
              -- ECase (Expr a) [Alter a]          
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
            do symbol "\\"
               vs <- some identifier
               character '.'
               e <- parseExpr
               return (ELam vs e)
              <|>
              -- atomic
              parseAExpr
              -- ==== expr1 ***
            --parseExpr1 --

{-
-- ***
-- expr1 -> expr2 || expr1 | expr2
-- OR 
-- associativity = right
parseExpr1 :: Parser (Expr Name) 
parseExpr1 = do e2 <- parseExpr2 -- ma non e' il tipo giusto!!
                character '|'
                e1 <- parseExpr1
                return (e1 ++ " | " ++ e2)
               <|> 
                parseExpr2

-- expr2 -> expr3 & expr2 | expr3
-- AND
-- associativity = right
parseExpr2 :: Parser (Expr Name)
parseExpr2 = do e3 <- parseExpr3
                character '&'
                e2 <- parseExpr2
                return (e3 ++ " & " ++ e2)
               <|> 
                parseExpr3

-- expr3 -> expr4 relop expr4 | expr 4
-- associativity = none
-- RELOP
-- relop = [< | <= | == | ~= | >= | >]
relop = ["<","<=","==","~=",">=",">"]
parseExpr3 :: Parser (Expr Name)
parseExpr3 = do el <- parseExpr4
                rel <- symbol any relop -- NICE TRY LUL
                er <- parseExpr4
                return (el ++ " " ++ rel ++ " " ++ er)
               <|> 
                parseExpr4 


-- expr4 -> expr5 + exp4 | expr5 - expr5 | expr5
-- ADDITION, DIFFERENCE
-- associativity = right, none
parseExpr4 :: Parser (Expr Name)
parseExpr4 = do e5 <- parseExpr5
                character '+'
                e4 <- parseExpr4
                return (e5 ++ " + " ++ e4)
               <|>
                el <- parseExpr5
                character '-'
                er <- parseExpr5
                return (el ++ " - " ++ er)
               <|>
                parseExpr5


-- expr5 -> expr6 * expr5 | expr6 / expr6 | expr6
-- MULTIPLICATION, DIVISION
-- associativity = right, none
parseExpr5 :: Parser (Expr Name)
parseExpr5 = do e6 <- parseExpr6
                character '*'
                e5 <- parseExpr5
                return (e6 ++ " * " ++ e5)
               <|>
                el <- parseExpr6
                character '/'
                er <- parseExpr6
                return (el ++ " / " ++ er)
               <|>
                parseExpr6
-- expr6 -> aexpr_1... aepxr_n (n>=1)
-- associativity = left
-- APPLICATION
-- in pratica ritorna una lista di Expr Names
-- problema peculiare... 
-- se e' una, e' atomica (application to nothing? mmh), else e' un'applicazione
parseExpr6 :: Parser (Expr Name)
parseExpr6 = do es <- some parseAExpr
                return es
-}

{-
-- AExpr -> var | num | Pack{num,num} | (expr)
parseAExpr :: Parser (Expr Name)
parseAExpr = do v <- identifier
                return (EVar v)
               <|>
             do n <- integer -- # only works for integers -- fract?
                return (ENum n)
               <|>
             do c <- parseConstr
                return (c)
               <|>
             do character '('
                e <- parseExpr -- correct? mmmh
                character ')'
                return (e)
             -- ( expr ) 
          
-}


--[+ - * /    < <= == ~= >=      & | ]
-- POTENTIALLY VERY USELESS
-- turns out it was useless
{-
type Binop a = Name 
parseBin :: Parser Binop
              -- arithop
parseBin = do a <- character '+'
-}
              -- relop
              -- boolop

-- TEST
-- test_prog = "f = 3; g x y = let z = x in z; h x = case (let y = x in y) of  <1> -> 2 <2> -> 5"

{- to define
parseVar :: Parser ( ?? )         DONE
parseExpr :: Parser (Expr Name)   IN THE MAKING
parseAExpr :: Parser (Expr Name)  KINDA DONE
parseDef :: Parser (Def Name)     DONE
parseAlt :: Parser (Alter Name)   DONE?
-}