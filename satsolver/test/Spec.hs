module Main where

import qualified Data.Algorithm.SatSolver.Clause as C
import qualified Data.Algorithm.SatSolver.Lit as Lit

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

mk :: (Ord a) => [Lit.Lit a] -> C.Clause a
mk = C.Clause . sort . nub

unitTests = testGroup "All Unit Tests"
   [ 
     -- Clause tests
    --testCase "Check empty clause" $ C.isEmpty $ mk []  @?= True,

    testCase "Check unit clause" $ C.isUnit (C.Clause [Lit.mkNeg' "x1"]) @?= True,
    testCase "Check not unit clause" $ C.isUnit (C.Clause [Lit.mkNeg' "x1", Lit.mkNeg' "x2"]) @?= False,
     
    testCase "Check is monotone" $ C.isMonotone (C.Clause [Lit.mkPos' "x1", Lit.mkPos' "x2", Lit.mkPos' "x3"]) @?= True,
    testCase "Check not monotone" $ C.isMonotone (C.Clause [Lit.mkNeg' "x1", Lit.mkPos' "x2", Lit.mkNeg' "x3"]) @?= False,
     
     
    testCase "Check is not NegMonotone" $ C.isNegMonotone (C.Clause [Lit.mkNeg' "x1", Lit.mkPos' "x2", Lit.mkNeg' "x3"]) @?= False,
    testCase "Check is NegMonotone" $ C.isMonotone (C.Clause [Lit.mkNeg' "x1", Lit.mkNeg' "x2", Lit.mkNeg' "x3"]) @?= True,
     
    
    testCase "Check isPosMonotone" $ C.isMonotone (C.Clause [Lit.mkPos' "x1", Lit.mkPos' "x2", Lit.mkPos' "x3"]) @?= True,
    testCase "Check not isPosMonotone" $ C.isMonotone (C.Clause [Lit.mkNeg' "x1", Lit.mkPos' "x2", Lit.mkNeg' "x3"]) @?= False,

    testCase "Check size_1" $ C.size (C.Clause [Lit.mkNeg' "x1", Lit.mkPos' "x2", Lit.mkNeg' "x3"]) @?= 3,
    testCase "Check size_2" $ C.size (C.Clause [Lit.mkPos' i | i <- [1..100]]) @?= 100
    ]
