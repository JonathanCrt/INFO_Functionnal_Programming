module Main where

import Lib


myMirror :: Eq a => [a] -> [a] -> Bool 
myMirror xs ys = xs == reverse ys


main :: IO ()
main =  do
    xs <- getLine
    ys <- getLine
    print (myMirror xs ys)
            
