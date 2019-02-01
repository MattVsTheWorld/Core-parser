module FileReader where

import System.IO
import Parser
import Expr
import Parser_Utils
import PrettyPrinter

readF :: IO String
readF = do inh <- openFile "input.txt" ReadMode
           prog <- readloop inh
           hClose inh
           return prog

-- Parse input file
main :: IO (Program Name)
main = do inp <- readF
          return (comp (parse parseProg inp))

-- Pretty print parse created from input file          
pprintInput :: IO ()
pprintInput = prettyPrint main
                where prettyPrint prog = prog >>= \pg -> putStr(pprint pg)
                                 
comp :: [(Program Name, Name)] -> Program Name
comp []         = error "no parse"
comp [(e,[])]   = e
comp [(_,a)]    = error ("Doesn't use all input : " ++ a)

readloop :: Handle -> IO [Char]
readloop inh = do ineof <- hIsEOF inh
                  if ineof
                    then return []
                  else do
                        x  <- hGetLine inh
                        xs <- readloop inh
                        return (x++xs)

