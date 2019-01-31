module PrettyPrint where
import Expr

----------------------
--  Pretty Printer  -- 
----------------------
data IseqRep = INil
             | IStr String
             | IAppend IseqRep IseqRep
             | IIndent IseqRep
             | INewline

type  Iseq = IseqRep

iNil :: Iseq                        -- Emtpy iseq  
iNil = INil

iStr :: String -> Iseq              -- Turn string into iseq
iStr = IStr

iNum :: Int -> Iseq
iNum n = iStr (show n)

iAppend :: Iseq -> Iseq -> Iseq     -- Append two iseqs
iAppend seq1 seq2 = IAppend seq1 seq2

-- Ignoring indentantion for now
iNewline :: Iseq
iNewline = INewline

iIndent :: Iseq -> Iseq
iIndent = IIndent

iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq,0)]

genSpaces :: Int -> String
genSpaces n = replicate n ' '

-- Function flatten
-- @Takes   : current column, work list
-- @Returns : result of concatenating each of the iseq in the list
-- Accumulates pending work. Manipulates representation type rather than abstract type
-- "By case" analysis: work-list
flatten :: Int -> 
    [(IseqRep,Int)] ->
    String
flatten _    []  = ""
flatten _   ((INewline,indent) : seqs) = "\n" ++ genSpaces indent ++ flatten indent seqs
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IIndent s, _) : seqs) = flatten col ((s,col + 2) : seqs)
flatten col ((IStr s, _) : seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2,indent) : seqs) = flatten col ((seq1,indent) : (seq2,indent) : seqs)

-- support functions to for the pretty printer
iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _   []  = INil
iInterleave _   [i] = i      -- no 'ins' at end of list
iInterleave ins (i:is) = (i `iAppend`  ins) `iAppend` iInterleave ins is

-- FixedWith Number	: Display a number with a fixed width
iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr (genSpaces (width - length digits) ++ digits)
  where
    digits = show n

-- Layout numbered: Layout a list of Iseqs numbered with 1) 2) 3) etc
iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (zipWith lay_item  [1..] seqs)
  where
    lay_item n iseq = iConcat [iFWNum 4 n, iStr ") ", iIndent iseq, iNewline]



---------------------------------------------
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _)   =  True
isAtomicExpr (ENum _)   =  True
isAtomicExpr _          =  False
---------------------------------------------
-- Define a Pretty Printer for the Core Language
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n)    = iNum n
pprExpr (EVar v)    = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1 `iAppend` iStr " ") `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline,
              iIndent (pprDefns defns), iNewline,
              iIndent (iStr "in "), pprExpr expr]
 where
  keyword
     | isrec == Recursive     = "letrec"
     | otherwise              = "let"
pprExpr (ELam vars expr) = iStr "\\ " `iAppend` iInterleave (iStr " ") (map iStr vars) `iAppend` iStr " . " `iAppend` pprExpr expr
pprExpr (EConstr tag arity) = iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]
-- pprExpr (ECase expr as) = iStr "case " `iAppend` pprExpr expr `iAppend` iStr " of " `iAppend` iIndent (pprCases as)
pprExpr (ECase expr as) = iConcat [iStr "case ", pprExpr expr, iStr " of ", iIndent (pprCases as)]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
   | isAtomicExpr e  = pprExpr e
   | otherwise       = (iStr "(" `iAppend` pprExpr e) `iAppend` iStr ")"

pprDefns :: [(String,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
   where
   sep = iConcat [iStr ";" , iNewline]

pprDefn :: (String, CoreExpr) -> Iseq
pprDefn (name, expr) = 
   iConcat [iStr name, iStr " ", iStr " = ",  iIndent(pprExpr expr) ]

pprProgram :: [CoreScDef]  -> Iseq
pprProgram  scDefs  = iInterleave sep (map pprScDef scDefs)
   where
   sep = iConcat [iStr ";" , iNewline]

pprScDef :: (String, [String], CoreExpr) -> Iseq
pprScDef (name, vars ,expr) =
   iConcat [iStr name, iStr " ",
   iInterleave (iStr " ") (map iStr vars), iStr " = ", iIndent(pprExpr expr)]

pprCases :: [Alter String] -> Iseq
pprCases as = iInterleave sep (map pprCase as)
  where 
    sep = iConcat [ iStr ";", iNewline ]

pprCase :: Alter String -> Iseq
pprCase (tag, as, expr) = prTag `iAppend` iInterleave (iStr " ") (map iStr as) `iAppend` (iStr "-> ") `iAppend` pprExpr expr
  where 
    prTag = iStr "<" `iAppend` iStr (show tag) `iAppend` iStr "> "

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

{-

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

pprExpr (ELet isrec defns expr) 
        = iConcat [ iStr keyword, iNewline, -- let / letrec
                    iStr "  ", iIndent (pprDefns defns), iNewline, -- Space and indent definition
                    iStr "in ", pprExpr expr] -- body
        where
            keyword | isrec == NonRecursive = "let"
                    | isrec == Recursive    = "letrec"

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
--iInterleave iseq [x] = x ## -- Notably, without this it puts iNil at the end?
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
    -}-}