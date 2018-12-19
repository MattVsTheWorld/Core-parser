module Parser where

import Expr
import Parser_Utils

import Control.Applicative
import Data.Char

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
parseScDef = do v  <- parseVar      -- DEFINE 
                 pf <- many parseVar -- DEFINE
                 character '='       -- throw away
                 body <- parseExpr   -- DEFINE
                 return (v, pf, body)
-}

-- now calls ident which considers '_'
-- Need Parser (EVar Name), not Parser String
exprIdent :: Parser (Expr Name) 
exprIdent = do x  <- lower
               xs <- many varch
               let a = EVar (x:xs) -- make into evar
               return a

parseVar :: Parser (Expr Name)
parseVar = token exprIdent

parseNum :: Parser (Expr Name)
parseNum = do xs <- some digit -- 1 or more digits
              return (ENum (read xs)) -- read per trasformarlo in Int
{-
-- Def -> var = expr
parseDef :: Parser (Def Name)
parseDef = do v <- parseVar
              character '='
              body = parseExpr
              return (v, body)
-}

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
parseAExpr = do v <- parseVar
                return (v)
               <|>
             do n <- parseNum
                return (n)
               <|>
             do c <- parseConstr
                return (c)
             -- ( expr ) 

{-            
--parseExpr :: Parser (Expr Name)
parseExpr :: Parser CoreExpr
-- let
parseExpr = do 
-}
{- to define
parseVar :: Parser ( ?? )
parseExpr :: Parser (Expr Name)

parseAExpr :: Parser (Expr Name)

parseDef :: Parser (Def Name)

parseAlt :: Parser (Alter Name)
-}