module Lib
    ( someFunc
    , checkWin
    , XO(..)
    , GameState(..)
    , Game(..)
    , makeBestDecision
    , matrixStart
    , nextStepMatrix
    , interactiveGame
    ) where

import Data.Matrix hiding (trace)
import Data.List (nub, sortOn)
import Debug.Trace
import qualified Data.HashMap.Strict as H
import Control.Monad.Par

data XO = X | O | Null deriving (Show, Eq, Read)

swapXO :: XO -> XO
swapXO X    = O
swapXO O    = X
swapXO Null = error "we can not have Null here"

data GameState = Win | Losing | Draw | Continue deriving (Show, Eq, Read)

data Game = Game
    { state :: GameState
    , returnMatrix :: [Matrix XO]
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

createBlankMatrix :: Int -> Matrix XO
createBlankMatrix n = matrix n n (\(_,_) -> Null)

checkWin :: Matrix XO -> XO
checkWin matrix =
    case nub $  filter (\case Null -> False; _ -> True) xoList of
        []   -> Null
        [xo] -> xo
        _    -> error "We can not have more than one win"

    where
        xoList = [checkRowsAndDiagonal xo n m | xo <- [X, O], m <- [matrix, transpose matrix]]
        n = nrows matrix
        checkRowsAndDiagonal xo n matrix = let
                rowsWin = any id [all (==xo) (getRow i matrix)| i <- [1..n]]
                diagWin = all (==xo) $ getDiag matrix
                anotherDiagWin = all (==xo) $ map (\(i, j) -> getElem i j matrix) $ zip [1..n] [n, n-1 .. 1]
            in
                if rowsWin || diagWin || anotherDiagWin
                then xo
                else Null

checkFull :: Matrix XO -> Bool
checkFull matrix
    | null filtered = True
    | otherwise     = False
        where
            filtered = filter (\case Null -> True; _ -> False) lst
            lst = toList matrix

findNulls :: Matrix XO -> [(Int, Int)]
findNulls matrix = [(i, j) | i <- [1..n], j <- [1..n], getElem i j matrix == Null]
    where
        n = nrows matrix


checkGameState :: Matrix XO -> XO -> GameState
checkGameState matrix xo
    | checkWin matrix == Null && checkFull matrix = Draw
    | checkWin matrix == xo                       = Win
    | checkWin matrix == swapXO xo                = Losing
    | otherwise                                   = Continue

xoToStr :: XO -> [Char]
xoToStr X = "2"
xoToStr O = "1"
xoToStr Null = "0"

maximizeToStr :: Bool -> [Char]
maximizeToStr True = "1"
maximizeToStr False = "0"

hashFunction :: XO -> Matrix XO -> Bool -> Int
hashFunction xo matrix maximize = read bigString :: Int
    where
        matrixList = concatMap xoToStr $ toList matrix
        bigString = xoToStr xo ++ maximizeToStr maximize ++ matrixList

makeBestDecision :: XO -> [Matrix XO] -> Bool -> Game
makeBestDecision xo matrix@(m:ms) maximize = case checkGameState m (if maximize then xo else swapXO xo) of
        Draw -> Game Draw matrix
        Win -> Game Win matrix
        Losing -> Game Losing matrix
        Continue -> let
                freePlaces = findNulls m
                newMatrixList = map (\coords -> setElem xo coords m) freePlaces
                notXO = swapXO xo
                decisions = map (\newM -> makeBestDecision notXO (newM : matrix) (not maximize)) newMatrixList
                sorted = if maximize
                    then reverse $ sortOn state decisions
                    else sortOn state decisions
                getResult lst = head $ sortOn (length . returnMatrix) $ filterSame lst
            in
                case sorted of
                    [] -> error "We can not have blank list"
                    lst -> getResult lst


filterSame :: [Game] -> [Game]
filterSame lst@(Game Win _ : _) = filter (\case Game Win _ -> True; Game _ _ -> False) lst
filterSame lst@(Game Draw _ : _) = filter (\case Game Draw _ -> True; Game _ _ -> False) lst
filterSame lst@(Game Continue _ : _) = filter (\case Game Continue _ -> True; Game _ _ -> False) lst
filterSame lst@(Game Losing _ : _) = filter (\case Game Losing _ -> True; Game _ _ -> False) lst


nextStepMatrix :: XO -> Matrix XO -> Matrix XO
nextStepMatrix xo matrix = newMatrix
    where
        matrixList = returnMatrix $ makeBestDecision xo [matrix] True
        newMatrix = head $ tail $ reverse matrixList

matrixStart :: Matrix XO
matrixStart = fromLists [[Null, Null, O], [Null, X, X], [Null, Null, O]]

interactiveGame :: IO ()
interactiveGame = do
    print "Choose X or O"
    xoStr <- getLine
    let xo = case read xoStr :: XO of
               Null -> error "It can not be NULL"
               xo   -> xo
    print "Choose 1 or 2"
    positionStr <- getLine
    let position = read positionStr :: Int
    print "Choose size of game field"
    nStr <- getLine
    let n = read nStr :: Int
    let xoStart = [xo, swapXO xo] !! (position - 1)
    let startMatrix = createBlankMatrix n
    result <- gameProcess xoStart xo startMatrix
    print result
        where
            gameProcess :: XO -> XO -> Matrix XO -> IO [Char]
            gameProcess xo humanXO matrix = case checkGameState matrix xo of
                Win -> return $ show xo ++ " win!!!"
                Draw -> return $ "Draw("
                Losing -> return $ show xo ++ " lose((("
                Continue -> if xo == humanXO
                    then (\x -> gameProcess (swapXO xo) humanXO (setElem humanXO x matrix)) =<< getCoordinates matrix
                    else gameProcess (swapXO xo) humanXO (nextStepMatrix xo matrix)
            getCoordinates matrix = do
                print matrix
                print "Your turn, input x and y"
                xyStr <- getLine
                let xyList = map (\x -> read x :: Int) $ take 2 $ words xyStr
                return (head xyList, head $ tail xyList)
