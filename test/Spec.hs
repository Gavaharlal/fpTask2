import Test.HUnit

import Lib
import Data.Matrix

main :: IO ()
main = runTestTT tests >> return ()


test31 :: Test
test31 = let
        matrix = fromLists
            [ [Null, Null, Null]
            , [Null, Null, Null]
            , [Null, Null, Null]
            ]
    in
        TestCase $ assertEqual "Not win size 3" (checkWin matrix) Null

test32 :: Test
test32 = let
        matrix = fromLists
            [ [X, Null, Null]
            , [Null, X, Null]
            , [Null, Null, X]
            ]
    in
        TestCase $ assertEqual "Win diag 3" (checkWin matrix) X

test33 :: Test
test33 = let
        matrix = fromLists
            [ [Null, Null, X]
            , [Null, X, Null]
            , [X, Null, Null]
            ]
    in
        TestCase $ assertEqual "Win another diag 3" (checkWin matrix) X

test34 :: Test
test34 = let
        matrix = fromLists
            [ [   X,    X,    X]
            , [Null, Null, Null]
            , [Null, Null, Null]
            ]
    in
        TestCase $ assertEqual "Win horizontal size 3" (checkWin matrix) X

test35 :: Test
test35 = let
        matrix = fromLists
            [ [O,    X,    X]
            , [O, Null, Null]
            , [O, Null, Null]
            ]
    in
        TestCase $ assertEqual "Win vertical size 3" (checkWin matrix) O

test36 :: Test
test36 = let
        matrix = fromLists
            [ [X, X, O]
            , [O, O, X]
            , [X, X, O]
            ]
    in
        TestCase $ assertEqual "Not win size 3" Null (checkWin matrix)

testDecision1 :: Test
testDecision1 = let
        matrixStart = fromLists
            [ [   X,    O,    O]
            , [Null, Null,    X]
            , [   X,    O,    O]
            ]
        matrixEnd = nextStepMatrix X matrixStart
    in
        TestCase $ assertEqual "X must win" X (checkWin matrixEnd)

tests :: Test
tests = TestList [ TestLabel "check win" test31
                 , TestLabel "check win" test32
                 , TestLabel "check win" test33
                 , TestLabel "check win" test34
                 , TestLabel "check win" test35
                 , TestLabel "check win" test36
                 , TestLabel "check makeBestDecision" testDecision1
                 ]
