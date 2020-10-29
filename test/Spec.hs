module Spec where

import Test.HUnit

import Algorithm
import Control.Parallel.Strategies
import System.Random
import Control.Monad.IO.Class
import Requests
import Config

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()


test31 = let
        matrix =
            [ [N, N, N]
            , [N, N, N]
            , [N, N, N]
            ]
    in
        TestCase $ assertEqual "Not win size 3" (checkWin matrix) N

test32 = let
        matrix =
            [ [X, N, N]
            , [N, X, N]
            , [N, N, X]
            ]
    in
        TestCase $ assertEqual "Win diag 3" (checkWin matrix) X

test33 = let
        matrix =
            [ [N, N, X]
            , [N, X, N]
            , [X, N, N]
            ]
    in
        TestCase $ assertEqual "Win another diag 3" (checkWin matrix) X

test34 = let
        matrix =
            [ [X, X, X]
            , [N, N, N]
            , [N, N, N]
            ]
    in
        TestCase $ assertEqual "Win horizontal size 3" (checkWin matrix) X

test35 = let
        matrix =
            [ [O, X, X]
            , [O, N, N]
            , [O, N, N]
            ]
    in
        TestCase $ assertEqual "Win vertical size 3" (checkWin matrix) O

test36 = let
        matrix =
            [ [X, X, O]
            , [O, O, X]
            , [X, X, O]
            ]
    in
        TestCase $ assertEqual "Not win size 3" N (checkWin matrix)

test37 = let
        matrix =
            [ [O, X, O, O]
            , [O, O, X, O]
            , [X, X, O, O]
            , [X, X, O, O]
            ]
    in
        TestCase $ assertEqual "Not win size 3" O (checkWin matrix)

testDecision1 = let
        matrixStart =
            [ [X, O, O]
            , [N, N, X]
            , [X, O, O]
            ]
        matrixEnd = nextStepMatrix X matrixStart
    in
        TestCase $ assertEqual "X must win" X (checkWin matrixEnd)

testDecision2 = let
        matrixStart =
            [ [X, O, O, N]
            , [X, X, X, O]
            , [O, O, X, N]
            , [X, O, O, N]
            ]
        matrixEnd = nextStepMatrix X matrixStart
    in
        TestCase $ assertEqual "X must win" X (checkWin matrixEnd)

simplePrintMatrix matrix = putStrLn $ concatMap (\line -> concatMap (\x ->  show x ++ " ") line ++ "\n") matrix


testClients :: Int -> Assertion
testClients n = do
     clientsCreate <- sequenceA $ replicate n $ createSessionAPI 3 defaultHost defaultPort
     let randomCoords matrix = do
             let l = findNulls matrix
                 lenList = length l
             coordsI <- liftIO $ randomRIO (0, lenList - 1)
             return $ l !! coordsI

         playGame :: Int -> [[XO]] -> IO (Bool)
         playGame id matrixStart = do
             coords <- randomCoords matrixStart
             move <- makeMoveAPI id (fst coords) (snd coords) defaultHost defaultPort
             case move of
                 Right (newMatrix, Nothing) -> playGame id newMatrix
                 Right (newMatrix, Just _) -> do
                     putStrLn "\n"
                     print id
                     simplePrintMatrix newMatrix
                     return True
                 Left msg -> error msg

     results <- sequenceA $ parMap rpar (\(Right (matrix, id', _)) -> playGame id' matrix) clientsCreate
     assertEqual (show n ++ " clients test") True $ and results


tests = TestList [
                   TestLabel "check win" test31
                 , TestLabel "check win" test32
                 , TestLabel "check win" test33
                 , TestLabel "check win" test34
                 , TestLabel "check win" test35
                 , TestLabel "check win" test36
                 , TestLabel "check win" test37
                 , TestLabel "check makeBestDecision" testDecision1
                 , TestLabel "check makeBestDecision 4" testDecision2
                 , TestCase (testClients 100)
                 ]
