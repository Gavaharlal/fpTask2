module Main where

import Brick.AttrMap
import Brick.Main
import Brick.Types hiding (Result)
import Brick.Widgets.Core
import Brick.Widgets.Center
import Graphics.Vty.Input.Events
import Brick.Widgets.Border

import Data.Char (isDigit)
import Algorithm (XO(..), getElem)
import Prelude hiding (id, error)
import qualified Graphics.Vty as V
import Brick.Util (on)
import Requests
import Config

main :: IO ()
main = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  return ()

data ProgramState =
    AddressInput
  | PortInput { portStr :: String }
  | SizeInput { sizeStr :: String }
  | Game
  | End {win :: XO}
  | Error {errorMsg :: String}
  deriving (Show, Eq)

data TuiState = TuiState
    { programState :: ProgramState
    , address :: String
    , port :: Int
    , size :: Int
    , matrix :: [[XO]]
    , id :: Int
    , humanXO :: XO
    , coords :: (Int, Int)
    }
    deriving (Show, Eq)

startTuiState :: TuiState
startTuiState = TuiState AddressInput "" defaultPort 0 [[]] 0 N (1, 1)

data ResourceName = ResourceName deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const theMap --const $ attrMap mempty []
    }

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (attrName "standart",                   V.white `on` V.black)
    , (attrName "highlight",                   V.red `on` V.black)
    ]

buildInitialState :: IO TuiState
buildInitialState = pure startTuiState

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts@TuiState {programState=AddressInput, address}
    = [center $ padAll 4 $ titleAndText "Input address and press enter:\n"$ address]

drawTui ts@TuiState {programState=PortInput portStr}
    = [center $ padAll 4 $ titleAndText "Input port and press enter:\n"$ portStr]

drawTui ts@TuiState {programState=SizeInput sizeStr}
    = [center $ padAll 4 $ titleAndText "Input size and press enter:\n"$ sizeStr]

drawTui ts@TuiState {programState=Game, id, matrix, coords, size, humanXO}
  | id == 0 = [center $ padAll 4 $ str $ "Press Enter to start game!" ]
  | otherwise = [center $ padAll 4 $ str title <=> printMatrix zipped coords ]
      where
          coordsMatrix = [[(i, j)| j <- [1..size]] | i <- [1..size]]
          zipped = map (\(l, lCoords) -> zip l lCoords) $ zip matrix coordsMatrix
          title = "Have to put " ++ (show humanXO) ++ "!"

drawTui ts@TuiState {programState=End xo, matrix, humanXO}
    = [center $ padAll 4 $
          hCenter ( str title ) <=>
          hCenter (printMatrixSimple matrix) <=>
          hCenter ( str "Input q => for quit " ) <=>
          hCenter ( str "and r => for restart" )
      ]
        where
            title = case xo of
              N -> "Draw"
              _ -> if xo == humanXO then "Win" else "Lose"

drawTui ts@TuiState {programState=Error errorMsg}
    = [center $ padAll 4 $ titleAndText "ERROR"$ errorMsg]

titleAndText :: [Char] -> [Char] -> Widget n
titleAndText title text = border $ ttitle <+> ttext
    where
        ttitle = str title
        ttext = str text

printMatrixSimple :: [[XO]] -> Widget n
printMatrixSimple matrix =
    joinBorders $
        vBox $
        map
        ( hBox .
            ( map
                 (\case
                     N -> border $ str "   "
                     X -> border $ str " X "
                     O -> border $ str " O "
                 )
            )
        ) matrix

printMatrix :: Eq a => [[(XO, a)]] -> a -> Widget n
printMatrix matrix currentCoords =
    joinBorders $
        vBox $
        map
        ( hBox .
            ( map
                 (\case
                     (N, coords) -> highlightPrint coords currentCoords "   "
                     (X, coords) -> highlightPrint coords currentCoords " X "
                     (O, coords) -> highlightPrint coords currentCoords " O "
                 )
            )
        ) matrix
    where
        highlightPrint c1 c2 string =
            if c1 == c2
            then (withAttr (attrName "highlight")) $ border $ str string
            else border $ str string



handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case programState s of
    AddressInput -> addressInput s e
    PortInput _ -> portInput s e
    SizeInput _ -> sizeInput s e
    Error _ -> continue $ startTuiState
    Game ->
      if id s == 0
          then do
              result <- createSessionAPI (size s) (address s) (port s)
              case result of
                  Left errorMsg -> continue $ s { programState=Error errorMsg }
                  Right (newMatrix, id, humanXO) -> continue (s { matrix=newMatrix, id=id, humanXO=humanXO })
          else
              makeMove s e
    End xo -> gameEnd s e

makeMove :: TuiState -> BrickEvent n1 e -> EventM n2 (Next TuiState)
makeMove state@TuiState{coords=(x, y), size, matrix, id, address, port} event =
  case event of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt state
        EvKey KUp [] -> continue $ state {coords=(x', y)}
              where
                  x' = if x - 1 <= 0 then size else x - 1

        EvKey KDown [] -> continue $ state {coords=(x', y)}
              where
                  x' = if x + 1 > size then 1 else x + 1

        EvKey KLeft [] -> continue $ state {coords=(x, y')}
              where
                  y' = if y - 1 <= 0 then size else y - 1

        EvKey KRight [] -> continue $ state {coords=(x, y')}
              where
                  y' = if y + 1 > size then 1 else y + 1

        EvKey KEnter [] -> case getElem x y matrix of
            N -> do
                result <- makeMoveAPI id x y address port
                case result of
                    Left errorMsg -> continue $ state { programState=Error errorMsg }
                    Right (newMatrix, Nothing) -> continue $ state {programState=Game, matrix=newMatrix}
                    Right (newMatrix, Just xo) -> continue $ state {programState=End xo, matrix=newMatrix}
            otherwise -> continue state
        _ -> continue state


addressInput :: TuiState -> BrickEvent n1 e -> EventM n2 (Next TuiState)
addressInput state@TuiState {programState=AddressInput, address} event =
  case event of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt state
        EvKey KDel [] -> continue $ state { programState=AddressInput, address=init address}
        EvKey (KChar c) [] -> continue $ state { programState=AddressInput, address=address ++ [c]}
        EvKey KEnter [] -> continue state {programState=PortInput ""}
        _ -> continue state
    _ -> continue state
    
portInput :: TuiState -> BrickEvent n1 e -> EventM n2 (Next TuiState)
portInput state@TuiState {programState=PortInput portStr} event =
  case event of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt state
        EvKey (KChar c) [] -> continue $ state { programState=PortInput (portStr ++ [c])}
        EvKey KEnter [] ->
                if all isDigit portStr
                    then continue $ state {programState=SizeInput "", port=read portStr :: Int}
                    else continue $ state {programState=PortInput ""}
        _ -> continue state
    _ -> continue state

sizeInput :: TuiState -> BrickEvent n1 e -> EventM n2 (Next TuiState)
sizeInput state@TuiState {programState=SizeInput sizeStr} event =
  case event of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt state
        EvKey (KChar c) [] -> continue $ state { programState=SizeInput (sizeStr ++ [c])}
        EvKey KEnter [] ->
                if all isDigit sizeStr
                    then continue $ state {programState=Game, size=read sizeStr :: Int}
                    else continue $ state {programState=SizeInput ""}
        _ -> continue state
    _ -> continue state

gameEnd :: TuiState -> BrickEvent n1 e -> EventM n2 (Next TuiState)
gameEnd state@TuiState {programState=End _} event =
  case event of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt state
        EvKey (KChar 'r') [] -> continue $ startTuiState
        _ -> continue state
    _ -> continue state

