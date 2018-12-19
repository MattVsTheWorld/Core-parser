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
parseVar :: Parser String
parseVar = identifier
{-
-- Def -> var = expr
parseDef :: Parser (Def Name)
parseDef = do v <- parseVar
              character '='
              body = parseExpr
              return (v, body)
-}
-- AExpr -> var | num | Pack{num,num} | (expr)
parseAExpr :: Parser String
parseAExpr = do v <- parseVar
                return (v)

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