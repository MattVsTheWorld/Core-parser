module Parser where

import Expr
import Parser_Utils

import Control.Applicative
import Data.Char

-- ESEMPI
{-
parseProg :: Parser (Program Name)
parseProg :: do p <- parseScDefn
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
parseScDefn :: Parser (ScDefn Name)
parseScDefn = do v  <- parseVar      -- DEFINE 
                 pf <- many parseVar -- DEFINE
                 character '='       -- throw away
                 body <- parseExpr   -- DEFINE
                 return (v, pf, body)

parseVar :: Parser String
parseVar = identifier -- now calls ident which considers '_'
              
--parseExpr :: Parser (Expr Name)
parseExpr :: Parser CoreExpr
parseExpr = 
  -}
{- to define
parseVar :: Parser ( ?? )
parseExpr :: Parser (Expr Name)

parseAExpr :: Parser (Expr Name)

parseDef :: Parser (Def Name)

parseAlt :: Parser (Alter Name)
-}