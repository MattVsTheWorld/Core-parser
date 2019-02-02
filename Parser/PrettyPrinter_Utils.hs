{-# OPTIONS_GHC -Wall #-}
module PrettyPrinter_Utils where

import Expr

data Iseq = INil
            | IStr String
            | IAppend Iseq Iseq
            | IIndent Iseq
            | INewline
                 deriving (Show)


-- Utility functions, used to transform and manipulate Iseqs.
iNil :: Iseq                         
iNil = INil

iStr :: String -> Iseq              
iStr = IStr

iNum :: Int -> Iseq
iNum = iStr.show

iAppend :: Iseq -> Iseq -> Iseq  
iAppend = IAppend

iNewline :: Iseq
iNewline = INewline

iIndent :: Iseq -> Iseq
iIndent = IIndent

iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq,0)]

space :: Int -> String
space n = replicate n ' '

-- Function flatten
-- @Params  : current column, work list.
-- @Returns : result of concatenating each of the Iseq in the list.
-- Accumulates pending work. Manipulates representation type rather than abstract type.
-- Work list contains pairs (Iseq, Num), where num is the required indentation.
flatten :: Int                     -- Current column; 0 for first column
            -> [(Iseq,Int)]        -- Work list
            -> String              -- Result
flatten _    []  = ""                                                                                 -- Worklist empty.
flatten _   ((INewline,indent) : seqs) = "\n" ++ (space indent) ++ (flatten indent seqs)              -- INewLine : perform indentation.
flatten col ((INil, _) : seqs) = flatten col seqs                                                     -- iNil     : skip.
flatten col ((IIndent s, _) : seqs) = flatten col ((s,col+4) : seqs)                                  -- IIndent  : set current indentation from current column. ~~~"artificial" indentation
flatten col ((IStr s, _) : seqs) = s ++ flatten col seqs                                              -- IStr     : append the string to the result of flattening the rest.
flatten col ((IAppend seq1 seq2,indent) : seqs) = flatten col ((seq1,indent) : (seq2,indent) : seqs)  -- IAppend  : flatten the workist where the two appended items are prepended.

-- Function iConcat
-- @Params  : List of Iseqs.
-- @Returns : A single Iseq, comprised of Iseqs concatenated using iAppend.
-- foldr naturally fits if seen as replacing (:) with iAppend and [] with iNil.
iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

-- Function iInterleave
-- @Params  : An Iseq and a list of Iseqs.
-- @Returns : A single Iseq, comprised of all elements of the list with
--            the single Iseq interleaved between each element.
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _   []  = INil
iInterleave _   [x] = x 
iInterleave iseq (x:xs) = (x `iAppend`  iseq) `iAppend` iInterleave iseq xs

-- Function isAtomicExpr
-- @Params  : an Expr.
-- @Returns : A Boolean value that specifies whether Expr is atomic or not.
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _)   =  True
isAtomicExpr (ENum _)   =  True
isAtomicExpr _          =  False
