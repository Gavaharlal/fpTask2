module Lib
    ( someFunc
    , checkWin
    , XO(..)
    , GameState(..)
    , Game(..)
    , makeBestDecision
    , nextStepMatrix
    , matrixStart
    ) where

import Data.Matrix hiding (trace)
import Data.List (nub, sortOn)
import Debug.Trace

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
  compare Losing Win = LT
  compare Losing Draw = LT
  compare Continue Win = LT
  compare Continue Draw = GT
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

makeBestDecision :: XO -> [Matrix XO] -> Bool -> Game
makeBestDecision xo matrix@(m:ms) maxim = case checkGameState m (if maxim then xo else swapXO xo) of
    Draw -> Game Draw matrix
    Win -> Game Win matrix
    Losing -> Game Losing matrix
    Continue -> let
            freePlaces = findNulls m
            newMatrixList = map (\coords -> setElem xo coords m) freePlaces
            decisions = map (\newM -> makeBestDecision (swapXO xo) (newM : matrix) (not maxim)) newMatrixList
            sorted = if maxim
              then reverse $ sortOn state decisions
              else sortOn state decisions
        in
            case sorted of
                [] -> error "We can not have blank list"
                lst -> head $
                  -- trace (show $ sortOn (length . returnMatrix) (filterSame lst))
                        (sortOn (length . returnMatrix) (filterSame lst))

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

-- makeBestDecision2 xo matrix@(m:ms) = let
--         freePlaces = findNulls m
--         newMatrixList = map (\coords -> setElem xo coords m) freePlaces
--         decisions = map (\newM -> makeBestDecision xo (newM : matrix)) newMatrixList
--         sorted = sortOn state decisions
--     in
--   case checkGameState m xo of
--     Draw -> Game Draw matrix
--     Win -> Game Win matrix
--     Losing -> Game Losing matrix
--     Continue -> let
--         in
--             case sorted of
--                 [] -> error "We can not have blank list"
--                 (m1:_) -> m1
