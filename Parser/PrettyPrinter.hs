{-# OPTIONS_GHC -Wall #-}
module PrettyPrinter where
import Expr

data Iseq = INil
             | IStr String
             | IAppend Iseq Iseq
             | IIndent Iseq
             | INewline
                  deriving (Show)

-- Emtpy iseq 
iNil :: Iseq                         
iNil = INil

-- Turn string into iseq
iStr :: String -> Iseq              
iStr = IStr

iNum :: Int -> Iseq
iNum n = iStr (show n)

iAppend :: Iseq -> Iseq -> Iseq  
iAppend seq1 seq2 = IAppend seq1 seq2

iNewline :: Iseq
iNewline = INewline

iIndent :: Iseq -> Iseq
iIndent s = IIndent s

iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq,0)]

space :: Int -> String
space n = replicate n ' '

-- Function flatten
-- @Params  : current column, work list.
-- @Returns : result of concatenating each of the iseq in the list.
-- Accumulates pending work. Manipulates representation type rather than abstract type.
-- Work list contains pairs (iseq, num), where num is the required indentation.
flatten :: Int                     -- Current column; 0 for first column
            -> [(Iseq,Int)]        -- Work list
            -> String              -- Result
flatten _    []  = ""
flatten _   ((INewline,indent) : seqs) = "\n" ++ (space indent) ++ (flatten indent seqs)
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IIndent s, _) : seqs) = flatten col ((s,col + 4) : seqs) -- "artificial" indentation
flatten col ((IStr s, _) : seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2,indent) : seqs) = flatten col ((seq1,indent) : (seq2,indent) : seqs)

-- Function iConcat
-- @Params  : List of Iseqs.
-- @Returns : A single iseq, comprised of iseqs concatenated using iAppend.
-- foldr naturally fits if seen as replacing (:) with iAppend and [] with iNil.
iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _   []  = INil
iInterleave _   [x] = x 
iInterleave iseq (x:xs) = (x `iAppend`  iseq) `iAppend` iInterleave iseq xs

-- FixedWith Number	: Display a number with a fixed width
iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr (space (width - length digits) ++ digits)
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

--binops :: [String]
--binops = ["+","-","*","/","<",">","<=",">=","~=","==","&","|"]

-- Define a Pretty Printer for the Core Language
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n)    = iNum n
pprExpr (EVar v)    = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1 `iAppend` iStr " ") `iAppend` pprAExpr e2
                    -- | e1 == EVar "+"  = (pprExpr (EVar "(+)") `iAppend` iStr " ") `iAppend` pprAExpr e2
                    -- | otherwise = (pprExpr e1 `iAppend` iStr " ") `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr)
         = iConcat [ iStr keyword, iNewline,              -- let / letrec
                     iIndent (pprDefns defns), iNewline,
                     iIndent (iStr "in "), pprExpr expr]
            where
               keyword | isrec == Recursive     = "letrec"
                       | otherwise              = "let"

pprExpr (ELam vars expr) = iStr "\\ " `iAppend` iInterleave (iStr " ") (map iStr vars) `iAppend` iStr " . " `iAppend` pprExpr expr
pprExpr (EConstr tag arity) = iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]
-- pprExpr (ECase expr as) = iStr "case " `iAppend` pprExpr expr `iAppend` iStr " of " `iAppend` iIndent (pprCases as)
pprExpr (ECase expr as) = iConcat [iStr "case ", pprExpr expr, iStr " of ", iIndent (pprCases as)]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
   | isAtomicExpr e  = pprExpr e
   | otherwise       = (iStr "(" `iAppend` pprExpr e) `iAppend` iStr ")"

-- Definitions separated by a semicolon
pprDefns :: [(String,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
   where sep = iConcat [ iStr ";" , iNewline ]

-- Definition is a var = expr
pprDefn :: (String, CoreExpr) -> Iseq
pprDefn (name, expr) = 
   iConcat [ iStr name, iStr " = ",  iIndent(pprExpr expr) ]

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