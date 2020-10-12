module Main where

import Lib
import Data.List

myMirror :: Eq a => [a] -> [a] -> Bool 
myMirror xs ys = xs == reverse ys

permute :: Eq a => [a] -> [a] -> Bool
permute [] [] = True
permute [] _ = False
-- delete x removes the first occurrence of x from its list argument.
permute (x:xs) ys = 
    x `elem` ys && permute xs (delete x ys)

permute' :: Ord a => [a] -> [a] -> Bool
permute' xs ys = sort xs == sort ys


--reversal :: Int -> Int -> [a] -> [a]


-- Prelude Data.List> permute' [1,2,4,8] [1,8,2,4]
--True


main :: IO ()
main =  do
    xs <- getLine
    ys <- getLine
    print (myMirror xs ys)
            
