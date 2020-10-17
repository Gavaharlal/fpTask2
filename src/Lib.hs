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
import Data.List (sortOn)

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
    case filter (\case Null -> False; _ -> True) xoList of
        []   -> Null
        [xo] -> xo
        _    -> error "We can not have more than one win"

    where
        xoList = [checkRowsAndDiagonal xo n matrix | xo <- [X, O]]
        n = nrows matrix
        checkRowsAndDiagonal xo n matrix = let
                rowsWin = or [all (==xo) (getRow i matrix)| i <- [1..n]]
                colsWin = or [all (==xo) (getCol i matrix)| i <- [1..n]]
                diagWin = all (==xo) $ map (\i -> getElem i i matrix) $ [1..n]
                anotherDiagWin = all (==xo) $ map (\(i, j) -> getElem i j matrix) $ zip [1..n] [n, n-1 .. 1]
            in
                if rowsWin || diagWin || anotherDiagWin || colsWin
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
    | winXO == Null && checkFull matrix = Draw
    | winXO == xo                       = Win
    | winXO == swapXO xo                = Losing
    | otherwise                         = Continue
      where
          winXO = checkWin matrix

-- checkGameStateM = memoize2 checkGameState

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


matrixStart :: Matrix XO
matrixStart = fromLists [[Null, Null, O], [Null, X, X], [Null, Null, O]]


interactiveGame :: IO ()
interactiveGame = gameProcessCycle
        where
            -- gameProcess :: XO -> XO -> Matrix XO -> IO [Char]
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
                print matrix
                print "Your turn, input x and y"
                xyStr <- getLine
                let xyList = map (\x -> read x :: Int) $ take 2 $ words xyStr
                return (head xyList, head $ tail xyList)

            gameProcessCycle = do
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
                print "Result : "
                print result
                print "Start new game? Y/n"
                continue <- getLine
                if continue == "Y"
                    then gameProcessCycle
                    else return ()



nextStepMatrix :: XO -> Matrix XO -> Matrix XO
nextStepMatrix xo matrix = newMatrix
    where
      newMatrix = setElem xo (makeBestDecision xo matrix True) $ matrix

makeBestDecision :: XO -> Matrix XO -> Bool -> (Int, Int)
makeBestDecision xo matrix maximize = let
        freePlaces = findNulls matrix
        checkPos xo coord matrix minMax =
            let
                newMatrix = setElem xo coord matrix
                minMaxXO = if not minMax then xo else swapXO xo
                checkPos' = map (\xy -> checkPos (swapXO xo) xy newMatrix (not minMax)) $ findNulls newMatrix
            in
                case checkGameState newMatrix minMaxXO of
                    Draw -> 0
                    Win -> 10
                    Losing -> (-10)
                    Continue -> if minMax
                        then maximum checkPos'
                        else minimum checkPos'
    in
        snd $ last $ sortOn fst $ map (\crd -> ((checkPos xo crd matrix (not maximize)), crd)) $ freePlaces

  -- case checkGameState matrix (if maximize then xo else swapXO xo) of
  --       Draw -> (Game Draw matrix, cash)
  --       Win -> (Game Win matrix, cash)
  --       Losing -> (Game Losing matrix, cash)
  --       Continue -> let
  --               freePlaces = findNulls m
  --               newMatrixList = map (\coords -> setElem xo coords m) freePlaces
  --               notXO = swapXO xo
  --               (decisions, cashes) = unzip $ map (\newM -> makeBestDecision notXO (newM : matrix) (not maximize) cash) newMatrixList
  --               newCash = foldl1 H.union cashes
  --               sorted = if maximize
  --                           then reverse $ sortOn state decisions
  --                           else sortOn state decisions
  --               getResult lst = head $ sortOn (length . returnMatrix) $ filterSame lst
  --               hash = hashFunction xo m maximize
  --           in
  --               if hash `H.member` cash
  --                   then (fromJust $ H.lookup hash cash, cash)
  --                   else case sorted of
  --                             [] -> error "We can not have blank list"
  --                             lst -> (getResult lst, newCash)
