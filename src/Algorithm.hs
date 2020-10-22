module Algorithm
    ( someFunc
    , checkWin
    , XO(..)
    , GameState(..)
    , Game(..)
    , makeBestDecision
    , matrixStart
    , nextStepMatrix
    , interactiveGame
    , createBlankMatrix
    , makeHumanMove
    , swapXO
    , checkGameState
    ) where

import Data.List (nub, sortOn, splitAt, transpose)
import Data.Maybe (fromJust)
import Data.Function.Memoize
import Control.Parallel.Strategies

data XO = X | O | N deriving (Eq, Show, Read)


swapXO :: XO -> XO
swapXO X    = O
swapXO O    = X
swapXO N = error "we can not have Null here"

deriveMemoizable ''XO

data GameState = Win | Losing | Draw | Continue deriving (Show, Eq, Read)

data Game = Game
    { state :: GameState
    , returnMatrix :: [[[XO]]]
    } deriving (Show, Eq)

instance Ord GameState where
  compare Win Draw = GT
  compare Win Losing = GT
  compare Win Continue = GT
  compare Draw Losing = GT
  compare Continue Draw  = GT
  compare Continue Losing = GT
  compare Draw Win= LT
  compare Draw Continue = LT
  compare Losing Win = LT
  compare Losing Draw = LT
  compare Continue Win = LT
  compare Losing Continue= LT
  compare Win Win = EQ
  compare Draw Draw = EQ
  compare Losing Losing = EQ
  compare Continue Continue = EQ

someFunc :: IO ()
someFunc = putStrLn "someFunc"

createBlankMatrix :: Int -> [[XO]]
createBlankMatrix n = replicate n $ replicate n N

setElemList :: a -> Int -> [a] -> [a]
setElemList e i lst = fsts ++ (e : snds)
    where
       (fsts', snds) = splitAt i lst
       fsts = init fsts'

getElem :: Int -> Int -> [[a]] -> a
getElem i j matrix = (matrix !! (i - 1)) !! (j - 1)

setElem :: a -> (Int, Int) -> [[a]] -> [[a]]
setElem e (i, j) matrix = matrix'
    where
        lst = matrix !! (i - 1)
        lst' = setElemList e j lst
        matrix' = setElemList lst' i matrix

getRow :: Int -> [a] -> a
getRow i matrix = matrix !! (i - 1)

checkWinM :: [[XO]] -> XO
checkWinM = memoize checkWin

checkWin :: [[XO]] -> XO
checkWin matrix =
    case nub $ filter (\case N -> False; _ -> True) xoList of
        []   -> N
        [xo] -> xo
        _    -> error "We can not have more than one win"

    where
        xoList = [checkRowsAndDiagonal xo n m | xo <- [X, O], m <- [matrix, transpose matrix]]
        n = length matrix
        checkRowsAndDiagonal xo n matrix = if win then xo else N
            where
                rowsWin = or [all (==xo) (getRow i matrix)| i <- [1..n]]
                diagWin = all (==xo) $ map (\i -> getElem i i matrix) $ [1..n]
                anotherDiagWin = all (==xo) $ map (\(i, j) -> getElem i j matrix) $ zip [1..n] [n, n-1 .. 1]
                win = runEval $ do
                    rWin <- rpar $ rowsWin
                    dWin <- rpar $ diagWin
                    adWin <- rpar $ anotherDiagWin
                    return $ rWin || dWin || adWin


checkFull :: Foldable t => t [XO] -> Bool
checkFull matrix
    | null filtered = True
    | otherwise     = False
        where
            filtered = filter (\case N -> True; _ -> False) lst
            lst = concat matrix

findNulls :: [[XO]] -> [(Int, Int)]
findNulls matrix = [(i, j) | i <- [1..n], j <- [1..n], getElem i j matrix == N]
    where
        n = length matrix


checkGameState :: [[XO]] -> XO -> GameState
checkGameState matrix xo
    | winXO == N && checkFull matrix = Draw
    | winXO == xo                       = Win
    | winXO == swapXO xo                = Losing
    | otherwise                         = Continue
      where
          winXO = checkWinM matrix


xoToStr :: XO -> [Char]
xoToStr X = "2"
xoToStr O = "1"
xoToStr N = "0"

maximizeToStr :: Bool -> [Char]
maximizeToStr True = "1"
maximizeToStr False = "0"

matrixStart :: [[XO]]
matrixStart = [[N, N, O], [N, X, X], [N, N, O]]

matrixShow :: (Foldable t, Show a) => t [a] -> IO ()
matrixShow matrix = putStrLn $ concatMap (\lst -> unwords (map show lst) ++ "\n") matrix


interactiveGame :: IO ()
interactiveGame = gameProcessCycle
        where
            gameProcess xo humanXO matrix = case checkGameState matrix xo of
                Win -> return $ show xo ++ " win!!!"
                Draw -> return $ "Draw("
                Losing -> return $ show xo ++ " lose((("
                Continue -> if xo == humanXO
                    then (\x -> gameProcess (swapXO xo) humanXO (setElem humanXO x matrix)) =<< getCoordinates matrix
                    else gameProcess (swapXO xo) humanXO newMatrix
                        where
                            newMatrix = nextStepMatrix xo matrix
            getCoordinates matrix = do
                matrixShow matrix
                print "Your turn, input x and y"
                xyStr <- getLine
                let xyList = map (\x -> read x :: Int) $ take 2 $ words xyStr
                return (head xyList, head $ tail xyList)

            gameProcessCycle = do
                print "Choose X or O"
                xoStr <- getLine
                let xo = case read xoStr :: XO of
                          N -> error "It can not be NULL"
                          xo   -> xo
                print "Choose 1 or 2"
                positionStr <- getLine
                let position = read positionStr :: Int
                print "Choose size of game field"
                nStr <- getLine
                let n = read nStr :: Int
                print position
                let xoStart = [xo, swapXO xo] !! (position - 1)
                print xoStart
                let startMatrix = createBlankMatrix n
                result <- gameProcess xoStart xo startMatrix
                print "Result : "
                print result
                print "Start new game? Y/n"
                continue <- getLine
                if continue == "Y"
                    then gameProcessCycle
                    else return ()


nextStepMatrix :: XO -> [[XO]] -> [[XO]]
nextStepMatrix xo matrix = newMatrix
    where
      newMatrix = setElem xo (makeBestDecision xo matrix True) $ matrix

makeBestDecision :: XO -> [[XO]] -> Bool -> (Int, Int)
makeBestDecision xo matrix maximize = let
        freePlaces = findNulls matrix
    in
        snd $ last $ sortOn fst $ map (\crd -> ((checkPos xo crd matrix (not maximize) 0), crd)) $ freePlaces

-- checkPosM = memoize4 checkPos


checkPos :: (Num a, Ord t, Ord a, Num t) => XO -> (Int, Int) -> [[XO]] -> Bool -> t -> a
checkPos xo coord matrix minMax depth =
    let
        newMatrix = setElem xo coord matrix
        minMaxXO = if not minMax then xo else swapXO xo
        checkPos' = parMap rpar (\xy -> checkPos (swapXO xo) xy newMatrix (not minMax) (depth + 1)) $ findNulls newMatrix
    in

      if depth > 4
      then (-1)
      else
        case checkGameState newMatrix minMaxXO of
            Draw -> 0
            Win -> 10
            Losing -> (-10)
            Continue -> if minMax
                then maximum checkPos'
                else minimum checkPos'
                
makeHumanMove :: a -> Int -> Int -> [[a]] -> [[a]]
makeHumanMove humanXO x y matrix = setElem humanXO (x, y) matrix
