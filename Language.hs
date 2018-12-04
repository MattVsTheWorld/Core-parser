module Language where
-- ## = to check
import Utils 

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

    
type CoreExpr = Expr Name  -- possibly useless

type Name = String

----------------
-- Let/Letrec --
----------------

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

-- Takes a list of definitions
-- Picks out the list of variables bound by the definitions
bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name,rhs) <- defns]

-- Takes a list of definitions
-- Extracts the list of right-hand sides to which they are bound
rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

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

-- Identifies expressions with no internal structure
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

-- a Core-language program is just a list of supercombinator definitions
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- a supercombinator definition contains
    -- (sueprcombinator definition = can define functions/CFA if no args)
        -- ~~ mathematical expression fully bound and self contained
    -- (1) name of the supercombinator = Name
    -- (2) its arguments = [a] (Can be empty)
    -- (3) its body = Expr a
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name


-----------------
--   Prelude   -- ## move
-----------------
{-
I x = x
K x y = x
K1 xy = y
S f g x = f x (g x)
compose f g x = f (g x)
twice f = compose f f
-}
preludeDefs :: CoreProgram
-- name, arguments, body
preludeDefs
    = [ ("I", ["x"], EVar "x"),
        ("K", ["x","y"], EVar "x"),
        ("K1", ["x","y"], EVar "y"),
        ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                                 (EAp (EVar "g") (EVar "x"))),
        ("compose", ["f","g","x"], EAp (EVar "f")
                                       (EAp (EVar "g") (EVar "x"))),
        ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

----------------------
--  Pretty Printer  -- ## move
----------------------

-- pprint :: CoreProgram -> String
-- Pre 1.5.2
{-
pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2
-}
-- to be continued ##
pprAExpr :: CoreExpr -> String
pprAExpr e -- ##
    | isAtomicExpr e    = pprExpr e
    | otherwise         = "(" ++ pprExpr e ++ ")"

-- demonstration of nasty performance
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
            where e2s = e2 : e2s

-- TESTING
-- :set +s
test1 = length (pprExpr (mkMultiAp 10 (EVar "f") (EVar "x")))
test2 = length (pprExpr (mkMultiAp 100 (EVar "f") (EVar "x")))
test3 = length (pprExpr (mkMultiAp 1000 (EVar "f") (EVar "x")))
test4 = length (pprExpr (mkMultiAp 10000 (EVar "f") (EVar "x"))) -- (2.24 secs, 8,591,972,104 bytes)
-- (pprExpr (mkMultiAp 15000 (EVar "f") (EVar "x"))) (7.06 secs, 19,621,938,760 bytes)
test5 = length (pprExpr (mkMultiAp 20000 (EVar "f") (EVar "x"))) --(25.14 secs, 35,049,543,328 bytes)

-- 1.5.2
-- Interface of the data type Iseq
-- operations which can be performed on the data type iseq
-- (+ type of such operations)

iNil :: Iseq                        -- Emtpy iseq   
iStr :: String -> Iseq              -- Turn string into iseq
iAppend :: Iseq -> Iseq -> Iseq     -- Append two iseqs
iNewline :: Iseq                    -- New line with indentation
iIndent :: Iseq -> Iseq             -- Indent an iseq
iDisplay :: Iseq -> String          -- Turn an iseq into a String

-- ++ has been replaced by iAppend
-- iStr added around literal strings
pprExpr :: CoreExpr -> Iseq
pprExpr (Evar v) = iStr v -- ~~ iStr turns v, a string, into an iSeq
pprExpr (EAp e1 e2) = (pprExpr e1) 'iAppend' (iStr " ") 'iAppend' (pprAExpr e2)