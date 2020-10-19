module Main where

import Lib
import Data.List

myMirror :: Eq a => [a] -> [a] -> Bool 
myMirror xs ys = xs == reverse ys

permute :: Eq a => [a] -> [a] -> Bool
permute [] [] = True
permute [] _ = False
-- delete x removes the first occurrence of x from its list argument.
permute (x:xs) ys
    -- if x is into ys I continue recursively 
    -- with x deletion
    | x `elem` ys = permute xs (delete x ys)
    | otherwise = False

permute' :: Ord a => [a] -> [a] -> Bool
permute' xs ys = sort xs == sort ys

-- Prelude Data.List> permute' [1,2,4,8] [1,8,2,4]
--True

-- zip = [a] -> [b] -> [(a, b)]
-- mapM_ -> display on multiline
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

pairs' :: [a] -> [(a, a)]
pairs' xs = zip xs $ tail xs

evenElts :: [a] -> [a]
evenElts  [] = []
evenElts [x] = [x]
evenElts [x,_] = [x]
evenElts (x:_:xs) = x: evenElts xs

subLength :: [[a]] -> [([a], Int)]
-- Applique la fonction (lambda) sur tous les elts de la liste
subLength = map (\ xs -> (xs, length xs))

appOnPairs :: (a ->c) -> (b -> d) -> [(a, b)] -> [(c, d)]
appOnPairs f g ((x, y) : xys) = (f x, g y): appOnPairs f g xys

appOnPairs' :: (a ->c) -> (b -> d) -> [(a, b)] -> [(c, d)]
appOnPairs' f g  = map(\ (x,y) -> (f x, g y))

--factors :: (Eq a) => [a] -> [[a]]
--factors [] = [[]]
--factors xs = tail (inits xs) ++ aux (tail xs)

subSeqs :: (Eq a) => [a] -> [[a]]
subSeqs [] = [[]]
subSeqs (x: xs) = xss ++ map (x:) xss
    where
        xss = subSeqs xs


subSeqs' :: (Eq a) => [a] -> [[a]]
subSeqs' (x:xs) = let ss = subSeqs' xs in ss ++ map (x:) ss

--reversal :: Int -> Int -> [a] -> [a]



main :: IO ()
main =  do
    xs <- getLine
    ys <- getLine
    print (myMirror xs ys)
            
