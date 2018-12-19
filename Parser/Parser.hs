module Parser where

import Expr
import Parser_Utils

import Control.Applicative
import Data.Char

{-
IMPORTANT NOTES:
- Only works with integer numbers so far
- atomic expression treatment of (expr) needs checking
-}

-- ESEMPI
{-
parseProg :: Parser (Program Name)
parseProg :: do p <- parseScDef
                do character ';'
                    ps <- parseProg
                    return (p:ps)
                <|> return [p]
-}

{-
Supercombinator = definizione di funzione
banana   x   xs     =       x + xs
  ^      ^   ^      ^       ^^^^^^
 Var    var var   char       expr
 Name      [a]               Expr a   
-}
{-
parseScDef :: Parser (ScDef Name)
parseScDef = do v  <- identifier      --= parseVar
                pf <- many identifier -- 
                character '='       -- throw away
                body <- parseExpr   -- DEFINE
                return (v, pf, body)
-}

-- now calls ident which considers '_'
{-
-- Need Parser (EVar Name), not Parser String
exprIdent :: Parser (Expr Name) 
exprIdent = do x  <- lower
               xs <- many varch
               let a = EVar (x:xs) -- make into evar
               return a

-- SUSPICION : should be string. wat do? FIXED
parseVar :: Parser (Expr Name)
parseVar = token exprIdent
-}
-- 
{- 
parseNum :: Parser (Expr Name)
parseNum = do xs <- some digit -- 1 or more digits
              return (ENum (read xs)) -- read per trasformarlo in Int

-}
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
                expr <- parseExpr -- correct? mmmh
                character ')'
                return (expr)
             --parseExpr  -- 
             -- ( expr ) 

parseExpr :: Parser (Expr Name)
-- let
  -- ELet IsRec [Def a] (Expr a)
parseExpr = do symbol "let" 
               defns <- some parseDef
               symbol "in"
               body <- parseExpr
               return (ELet NonRecursive defns body)
              <|>
            parseAExpr -- Atomic



-- letrec

-- case

-- lambda


{- to define
parseVar :: Parser ( ?? )         DONE
parseExpr :: Parser (Expr Name)   IN THE MAKING
parseAExpr :: Parser (Expr Name)  KINDA DONE
parseDef :: Parser (Def Name)     DONE
parseAlt :: Parser (Alter Name)   FUC
-}