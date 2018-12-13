module Expr where

-- page 17
data Expr a 
          = EVar Name                 -- Variables    
          | ENum Int                  -- Numbers
          | EConstr Int Int           -- Constructor tag arity
          | EAp (Expr a) (Expr a)     -- Applications
          | ELet                      -- Let(rec) expressions
            IsRec                       -- boolean with True = recursive
            [(a, Expr a)]               -- Definitions
            (Expr a)                    -- Body of let(rec)
          | ECase                     -- Case expression
            (Expr a)                    -- Expression to scrutinise
            [Alter a]                   -- Alternatives
          | ELam [a] (Expr a)         -- Lambda abstractions
    deriving (Show,Read) -- Text? ##

type CoreExpr = Expr Name
type Name = String

----------------
--    case    --
----------------
-- Case takes an expression to analyze + a list of alternatives
-- Each alternative contains 
    -- (1) a tag = Int
    -- (2) a list of bound variables = [a]
    -- (3) the expression to the right of the arrow
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- a Core-language program is just a list of supercombinator definitions
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- a supercombinator definition contains
    -- (supercombinator definition = can define functions/CFA if no args)
        -- ~~ mathematical expression fully bound and self contained
    -- (1) name of the supercombinator = Name
    -- (2) its arguments = [a] (Can be empty)
    -- (3) its body = Expr a
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

----------------
-- Let/Letrec --
----------------
{- mmmh
data IsRec = NonRecursive | recursive
        deriving Show
-}
type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False