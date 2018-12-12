module PreludeDefs where

import Expr

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