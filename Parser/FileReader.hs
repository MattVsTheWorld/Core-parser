module FileReader where

import System.IO
import Parser
import Expr
import Parser_Utils
import PrettyPrint

readF :: IO String
readF = do inh <- openFile "input.txt" ReadMode
           prog <- readloop inh
           hClose inh
           return prog

main :: IO (Program Name)
main = do inp <- readF
          return (comp (parse parseProg inp))

prettyPrint :: IO (Program Name) -> IO ()
prettyPrint pg = do prog <- pg
                    putStr (pprint prog)

test :: IO ()
test = prettyPrint main

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

