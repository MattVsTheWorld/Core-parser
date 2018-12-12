module Utils where

-- synonyms for Gofer prelude
shownum n = show n
hd :: [a] -> a
hd = head
tl :: [a] -> [a]
tl = tail
zip2 :: [a] -> [b] -> [(a,b)]
zip2 = zip

foldll :: (a -> b -> a) -> a -> [b] -> a
foldll = foldl -- ##