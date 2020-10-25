module Main where

import Brick.AttrMap
import Brick.Main
import Brick.Types hiding (Result)
import Brick.Widgets.Core
import Brick.Focus
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Graphics.Vty.Input.Events
import Data.Text (Text, pack,  unpack)
import Brick.Widgets.Border

import Data.Char (isDigit)
import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Aeson hiding (Error)
import qualified Data.HashMap.Strict as H
import Data.Vector (toList, Vector)
import Data.Maybe (fromJust)
import Algorithm (XO(..), getElem)
import Prelude hiding (id, error)
import qualified Graphics.Vty as V
import Brick.Util (on)

import Data.Aeson.TH
$(deriveJSON defaultOptions ''XO)

main :: IO ()
main = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data ProgramState =
    AddressInput
  | SizeInput { sizeStr :: String }
  | Game
  | End {win :: XO}
  | Error {errorMsg :: String}
  deriving (Show, Eq)

data TuiState = TuiState
    { programState :: ProgramState
    , address :: String
    , size :: Int
    , matrix :: [[XO]]
    , id :: Int
    , humanXO :: XO
    , coords :: (Int, Int)
    }
    deriving (Show, Eq)

startTuiState = TuiState AddressInput "" 0 [[]] 0 N (1, 1)

data ResourceName =
  ResourceName | EditPort
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const theMap --const $ attrMap mempty []
    }
theMap = attrMap V.defAttr
    [ (attrName "standart",                   V.white `on` V.black)
    , (attrName "highlight",                   V.red `on` V.black)
    ]

buildInitialState :: IO TuiState
buildInitialState = pure startTuiState

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts@TuiState {programState=AddressInput, address}
    = [center $ padAll 4 $ titleAndText "Input address and press enter:\n"$ address]

drawTui ts@TuiState {programState=SizeInput sizeStr}
    = [center $ padAll 4 $ titleAndText "Input size and press enter:\n"$ sizeStr]

drawTui ts@TuiState {programState=Game, id, matrix, coords, size, humanXO}
  | id == 0 = [center $ padAll 4 $ str $ "Press Enter to start game!" ]
  | otherwise = [center $ padAll 4 $ str title <=> printMatrix zipped coords ]
      where
          coordsMatrix = [[(i, j)| j <- [1..size]] | i <- [1..size]]
          zipped = map (\(l, lCoords) -> zip l lCoords) $ zip matrix coordsMatrix
          title = "Have to put " ++ (show humanXO) ++ "!"

drawTui ts@TuiState {programState=End xo, matrix}
    = [center $ padAll 4 $ str title <=> printMatrixSimple matrix <=> str "Input q => for quit \nand r => for restart"]
        where
            title = show xo ++ " win!"

drawTui ts@TuiState {programState=Error errorMsg}
    = [center $ padAll 4 $ titleAndText "ERROR"$ errorMsg]

titleAndText title text = border $ ttitle <+> ttext
    where
        ttitle = str title
        ttext = str text

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
    SizeInput _ -> sizeInput s e
    Error _ -> continue $ startTuiState
    Game ->
      if id s == 0
          then do
              result <- createSession (size s) (address s)
              case result of
                  Left errorMsg -> continue $ s { programState=Error errorMsg }
                  -- Right (newMatrix, id, humanXO) -> makeMove (s { matrix=newMatrix, id=id, humanXO=humanXO }) e
                  Right (newMatrix, id, humanXO) -> continue (s { matrix=newMatrix, id=id, humanXO=humanXO })
          else
              makeMove s e
    End xo -> gameEnd s e

makeMove state@TuiState{coords=(x, y), size, matrix, id, address} event =
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
                result <- makeMoveAPI id x y address
                case result of
                    Left errorMsg -> continue $ state { programState=Error errorMsg }
                    Right (newMatrix, Nothing) -> continue $ state {programState=Game, matrix=newMatrix}
                    Right (newMatrix, Just xo) -> continue $ state {programState=End xo, matrix=newMatrix}
            otherwise -> continue state
        _ -> continue state


createSession size address = runReq defaultHttpConfig $ do
  r <-
    req
      GET
      (http (pack address) /: "session-create")
      NoReqBody
      jsonResponse $
      ("size" =: (size :: Int))
  let requestData = (responseBody r :: Object)
      Success newMatrix = fromJSON (fromJust $ H.lookup "matrix" requestData) :: Result [[XO]]
      Success id = fromJSON (fromJust $ H.lookup "id" requestData) :: Result Int
      Success humanXO = fromJSON (fromJust $ H.lookup "humanXO" requestData) :: Result XO
      error = fromJSON (fromJust $ H.lookup "error" requestData) :: Result String
      result = case error of
          Success errorMsg -> Left errorMsg
          _                -> Right (newMatrix, id, humanXO)
  return result

makeMoveAPI id x y address = runReq defaultHttpConfig $ do
  r <-
    req
      GET
      (http (pack address) /: "move")
      NoReqBody
      jsonResponse $
      ("id" =: (id :: Int)) <>
      ("x" =: (x :: Int)) <>
      ("y" =: (y :: Int))
  let requestData = (responseBody r :: Object)
      Success newMatrix = fromJSON (fromJust $ H.lookup "matrix" requestData) :: Result [[XO]]
      win = fromJSON (fromJust $ H.lookup "win" requestData) :: Result XO
      error = fromJSON (fromJust $ H.lookup "error" requestData) :: Result String
      result = case error of
          Success errorMsg -> Left errorMsg
          _                ->
              case win of
                  Success xo -> Right (newMatrix, Just xo)
                  _          -> Right (newMatrix, Nothing)
  return result

addressInput state@TuiState {programState=AddressInput, address} event =
  case event of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt state
        EvKey KDel [] -> continue $ state { programState=AddressInput, address=init address}
        EvKey (KChar c) [] -> continue $ state { programState=AddressInput, address=address ++ [c]}
        EvKey KEnter [] -> continue state {programState=SizeInput ""}
        _ -> continue state
    _ -> continue state

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

gameEnd state@TuiState {programState=End _} event =
  case event of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt state
        EvKey (KChar 'r') [] -> continue $ startTuiState
        _ -> continue state
    _ -> continue state

