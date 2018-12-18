module Parser where

import Expr
import Parser_Utils



-- ESEMPI

{-

parseProg :: Parser (Program Name)
parseProg :: do p <- parseScDef
                do character ';'
                    ps <- parseProg
                    return (p:ps)
                <|> return [p]

parseScDef :: Parser (ScDef Name)
parseScDef = do v  <- parseVar      -- DEFINE 
                pf <- many parseVar -- DEFINE
                character '='
                body <- parseExpr -- DEFINE
                return (v, pf, body)
-}

{- to define
parseVar :: Parser ( ?? )
parseExpr :: Parser (Expr Name)

parseAExpr :: Parser (Expr Name)

parseDef :: Parser (Def Name)

parseAlt :: Parser (Alter Name)
-}