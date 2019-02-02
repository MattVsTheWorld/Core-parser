{-# OPTIONS_GHC -Wall #-}
module PrettyPrinter where

import Expr
import PrettyPrinter_Utils

-- Pretty print expressions, case analysis.
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n)    = iNum n
pprExpr (EVar v)    = iStr v
pprExpr (EAp e1 e2) = iConcat [pprExpr e1, iStr " ", pprAExpr e2]
pprExpr (ELet isrec defns expr)
         = iConcat [ iStr keyword, iIndent (pprDefns defns), iNewline,     
                     iIndent (iStr "in "), pprExpr expr ]
            where
               keyword | isrec == Recursive     = "letrec"
                       | otherwise              = "let"
pprExpr (ELam vars expr) = 
   iConcat [ iStr "\\", iInterleave (iStr " ") (map iStr vars), iStr " . ", pprExpr expr]
pprExpr (EConstr tag arity) = 
   iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]
pprExpr (ECase expr as) = 
   iConcat [ iStr "case ", pprExpr expr, iStr " of ", iIndent (pprCases as) ]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
   | isAtomicExpr e  = pprExpr e
   | otherwise       = iConcat [ iStr "(", pprExpr e, iStr ")" ]

-- Semicolon and newline.
sep :: Iseq
sep = iConcat [ iStr ";", iNewline ]

-- A program is a sequence of supercombinators.
pprProgram :: [CoreScDef]  -> Iseq
pprProgram  scDefs  = iInterleave sep (map pprScDef scDefs)

-- A supercombinator has a name, an optional list of arguments and the body expression.
pprScDef :: (String, [String], CoreExpr) -> Iseq
pprScDef (name, [], expr)   =   iConcat [ iStr name, iStr " = ", iIndent(pprExpr expr) ]
pprScDef (name, vars, expr) =
   iConcat [ iStr name, iStr " ", iInterleave (iStr " ") (map iStr vars), 
            iStr " = ", iIndent(pprExpr expr) ]

-- Multiple definitions are separated by a semicolon and a newline.
-- Definitions start at a new line.
pprDefns :: [(String,CoreExpr)] -> Iseq
pprDefns defns = iConcat [iNewline, iInterleave sep (map pprDefn defns)]

-- A single definition is in the form var = expr.
pprDefn :: (String, CoreExpr) -> Iseq
pprDefn (name, expr) = 
   iConcat [ iStr name, iStr " = ",  iIndent(pprExpr expr) ]

-- Multiple cases are separated by a semicolon and a newline.
-- Cases start at a new line.
pprCases :: [Alter String] -> Iseq
pprCases alts = iConcat [iNewline, iInterleave sep (map pprCase alts)]

-- A single case is in the form <tag> vars -> expr.
pprCase :: Alter String -> Iseq
pprCase (tag, vars, expr) = 
   iConcat [ iConcat [ iStr "<", iStr (show tag), iStr "> "], iInterleave (iStr " ") (map iStr vars), (iStr "-> "), pprExpr expr ]

-- apply iDisplay at the top level, to the Iseq that represents the entire program.
pprint :: CoreProgram -> String
pprint = iDisplay.pprProgram