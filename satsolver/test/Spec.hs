module Spec where

import Test.HUnit
import qualified Data.Algorithm.SatSolver.Clause as C
import Data.List


testEmptyClause = assertBool "[]" C.isEmpty (c.mk[])


main :: IO ()
main = do
  putStrLn ""
  putStrLn $ if C.isEmpty (C.mk []) then "OK" else "FAIL!"
  return ()