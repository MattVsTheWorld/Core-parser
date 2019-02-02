{-# OPTIONS_GHC -Wall #-}
module Expr where

data Expr a 
            = EVar Name                 -- Variables    
            | ENum Int                  -- Numbers
            | EConstr Int Int           -- Constructor tag arity
            | EAp (Expr a) (Expr a)     -- Applications
            | ELet                      -- Let(rec) expressions
                IsRec                       -- NonRecursive | Recursive
                [Def a]                     -- Definitions
                (Expr a)                    -- Body of let(rec)
            | ECase                     -- Case expression
                (Expr a)                    -- Expression to scrutinise
                [Alter a]                   -- Alternatives
            | ELam [a] (Expr a)         -- Lambda abstractions
    deriving Show

type CoreExpr = Expr Name
type Name = String

-- Case takes an expression to analyze + a list of alternatives
-- Each alternative contains 
    -- (1) a tag = Int
    -- (2) a list of bound variables = [a]
    -- (3) the expression to the right of the arrow
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- a Core-language program is just a list of supercombinator definitions
type Program a = [ScDef a]
type CoreProgram = Program Name

-- a supercombinator definition contains
    -- (1) name of the supercombinator = Name
    -- (2) its arguments = [a] (Can be empty)
    -- (3) its body = Expr a
type ScDef a = (Name, [a], Expr a)
type CoreScDef = ScDef Name

-- Let/Letrec
data IsRec = NonRecursive | Recursive
        deriving (Show,Eq)

type Def a = (a, Expr a)
