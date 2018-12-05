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

    
type CoreExpr = Expr Name

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
{-
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False
-}

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

-- to be continued ##
pprAExpr :: CoreExpr -> String
pprAExpr e -- ##
    | isAtomicExpr e    = pprExpr e
    | otherwise         = "(" ++ pprExpr e ++ ")"
-}
{-
-- demonstration of nasty performance
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
            where e2s = e2 : e2s


data Iseq = INil
            | IStr String
            | IAppend Iseq Iseq
                deriving (Show) -- ##

iNil :: Iseq                        -- Emtpy iseq  
iNil = INil
iStr :: String -> Iseq              -- Turn string into iseq
iStr str = IStr str
iAppend :: Iseq -> Iseq -> Iseq     -- Append two iseqs
iAppend seq1 seq2 = IAppend seq1 seq2

-- Ignoring indentantion for now
iNewline :: Iseq                    -- New line with indentation
iNewline = iStr "\n"
iIndent :: Iseq -> Iseq             -- Indent an iseq
iIndent seq = seq

-- Goal: have iDisplay linear in size of iseq
-- We use the more general function flatten

-- Function flatten
-- @Takes   : list of iseq
-- @Returns : result of concatenating each of the iseq in the list
-- Accumulates pending work. Manipulates representation type rather than abstract type
-- "By case" analysis: work-list
flatten :: [Iseq] -> String
flatten [] = "" -- work list empty
flatten (INil : seqs) = flatten seqs  -- First element is INil
flatten (IStr s : seqs) = s++ (flatten seqs) -- ## First element IStr, append to flattened work list
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs) -- push one more item in front of the work list

iDisplay :: Iseq -> String          -- Turn an iseq into a String
iDisplay seq = flatten [seq]


isAtomicExpr :: CoreExpr -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False -- ?

pprAExpr :: CoreExpr -> Iseq
pprAExpr e -- ##
    | isAtomicExpr e    = pprExpr e
    | otherwise         = (iStr "(") `iAppend` pprExpr e `iAppend` (iStr ")")

-- ++ has been replaced by iAppend
-- iStr added around literal strings
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v -- ~~ iStr turns v, a string, into an iSeq
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)

-- extension of pprExpr to handle let and letrec
-- pprExpr for Let
-- (Let (local definition, is it recursive, Definition [(a, Expr a)])), body
{-
    | ELet                      -- Let(rec) expressions
            IsRec                       -- boolean with True = recursive
            [(a, Expr a)]               -- Definitions
            (Expr a)                    -- Body of let(rec)

aaa x y = let 
            r = 3*x 
              s = 6*y
              in  r + s

              
            -}
pprExpr (ELet isrec defns expr) 
        = iConcat [ iStr keyword, iNewline, -- let / letrec
                    iStr "  ", iIndent (pprDefns defns), iNewline, -- Space and indent definition
                    iStr "in ", pprExpr expr] -- body
        where
            keyword | not isrec = "let"
                    | isrec     = "letrec"

pprDefns :: [(Name,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns) -- maps comprehension of definition and puts a ;\n in between each
                    where sep = iConcat [ iStr ";", iNewline] -- sep = ;\n 

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
            = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr)]

-- EX 1.2
-- Take a list of iseqs and uses iAppend to concatenate into a single iseq            
iConcat :: [Iseq] -> Iseq   
iConcat [x] = x;  
iConcat (x:xs) = x `iAppend` (iConcat xs) 
iConcat _ = iNil; --  ##

-- similar, interleaves a specified iseq between each adjacent pair
-- [a, b, c, d]
-- x
-- [a, x, b, x, c, x, d]
iInterleave :: Iseq -> [Iseq] -> Iseq
--iInterleave iseq [x] = x ##
iInterleave iseq (x:xs) = x `iAppend` iseq `iAppend` (iInterleave iseq xs)
iInterleave _ _= iNil -- ##

-- Testing
a = iStr "Banana"
b = iAppend (iStr "apple") (iStr "mango")
c = [a,b]
d = iInterleave (IStr "LOL") c
{-
pprint :: CoreProgram -> String -- ##
pprint = iDisplay (pprProgram prog)
-}
-- EX 1.3
-- pprExpr for case


{-
         | ECase                     -- Case expression
            (Expr a)                    -- Expression to scrutinise
            [Alter a]                   -- Alternatives

            type Alter a = (Int, [a], Expr a)
            type CoreAlt = Alter Name

-- Case takes an expression to analyze + a list of alternatives
-- Each alternative contains 
    -- (1) a tag = Int
    -- (2) a list of bound variables = [a]
    -- (3) the expression to the right of the arrow

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-}
{-
pprExpr (ECase expr (c:cases))
            = iConcat [ iStr "case ", pprExpr expr, iStr " of", iNewLine,
                        iStr "  ", ] -- ## to be continued.

pprAlters :: [(Int, [a], CoreExpr)]

pprAlter :: (Int, [a], CoreExpr)
-}

-- pprExpr for lambda

-- pprAexpr

-- pprProgram

{-
pprAExpr :: CoreExpr -> String
pprAExpr e -- ##
    | isAtomicExpr e    = pprExpr e
    | otherwise         = "(" ++ pprExpr e ++ ")"
    -}