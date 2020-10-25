module Server
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.State
import Data.IORef
import qualified Data.Map as M
import Algorithm
import System.Random
import Prelude hiding (error)

type Matrix = [[XO]]

data SessionState = SessionState
  { matrix :: Matrix
  , aiXO :: XO
  } deriving (Show, Eq)

data GlobalState = GlobalState
    { freeIds :: [Int]
    , sessionsState :: M.Map Int SessionState
    } deriving (Show, Eq)

data ReturnData = ReturnData
    { matrix :: Maybe Matrix
    , error :: Maybe String
    , win :: Maybe XO
    } deriving (Show, Eq)

data FirstData = FirstData
    { matrix :: Maybe Matrix
    , id :: Maybe Int
    , humanXO :: Maybe XO
    , error :: Maybe String
    } deriving (Show, Eq)


$(deriveJSON defaultOptions ''ReturnData)
$(deriveJSON defaultOptions ''FirstData)
$(deriveJSON defaultOptions ''XO)

type API = "session-create" :> QueryParam "size" Int :> Get '[JSON] FirstData
    :<|> "move" :> QueryParam "id" Int :> QueryParam "x" Int :> QueryParam "y" Int :> Get '[JSON] ReturnData

type IOState = IORef GlobalState


startApp :: IO ()
startApp = run 8081 =<< app

globalStateStart :: GlobalState
globalStateStart = GlobalState [1..500] M.empty

app :: IO Application
app = (serve api . server) <$> newIORef globalStateStart

api :: Proxy API
api = Proxy

getId :: IOState -> Maybe Int -> Handler FirstData
getId ioState Nothing = return $ FirstData Nothing Nothing Nothing $ Just "do not have size parameter"
getId ioState (Just size)
    | size < 0 || size > 7 = return $ FirstData Nothing Nothing Nothing $ Just "size is not from 0 < size < 8"
    | otherwise = liftIO $ do
        GlobalState {freeIds, sessionsState} <- readIORef ioState
        randomBool <- randomIO :: IO Bool

        let updatedFreeIds = tail freeIds
            id = head freeIds
            aiXO = if randomBool then X else O
            matrix = if aiXO == X
                then nextStepMatrix aiXO $ createBlankMatrix size
                else createBlankMatrix size
            sessionsState' = M.insert id (SessionState matrix aiXO) sessionsState

        writeIORef ioState $ GlobalState {freeIds=updatedFreeIds, sessionsState=sessionsState'}
        return $ FirstData
            { matrix = Just matrix
            , id = Just id
            , humanXO = Just $ swapXO aiXO
            , error = Nothing
            }

returnError :: String -> ReturnData
returnError msg = ReturnData { matrix=Nothing, error=Just msg, win=Nothing }

move :: IOState -> Maybe Int -> Maybe Int -> Maybe Int -> Handler ReturnData
move ioState Nothing _ _ = return $ returnError "You not set session id"
move ioState _ Nothing _ = return $ returnError "x param error"
move ioState _ _ Nothing = return $ returnError "y param error"
move ioState (Just id) (Just x) (Just y) = liftIO $ do
    GlobalState {freeIds, sessionsState} <- readIORef ioState
    let returnValue =
          case M.lookup id sessionsState of
              Nothing -> returnError "can not find session with this id"
              Just SessionState {matrix, aiXO} -> if checkErrorX || checkErrorY
                  then returnError "x or y smaller than 1 or bigger than matrix size"
                  else checkGameStateF matrix humanXO True
                  where
                      checkGameStateF matrix xo firstTime = case checkGameState matrix xo of
                        Win -> ReturnData (Just matrix) Nothing (Just xo)
                        Draw -> ReturnData (Just matrix) Nothing (Just N)
                        Losing -> ReturnData (Just matrix) Nothing (Just aiXO)
                        Continue -> ReturnData (Just aiMovedMatrix) Nothing $
                            if firstTime
                            then win $ checkGameStateF aiMovedMatrix xo False
                            else Nothing
                            where
                                aiXO = swapXO xo
                                aiMovedMatrix = nextStepMatrix aiXO matrix'

                      matrix' = makeHumanMove humanXO x y matrix
                      humanXO = swapXO aiXO
                      size = length matrix
                      checkErrorX = x < 1 || x > size
                      checkErrorY = y < 1 || y > size
    case returnValue of
        rd@(ReturnData Nothing _ _) -> return rd
        rd@(ReturnData _ _ (Just _)) -> do
            let sessionsState' = M.delete id sessionsState
                freeIds' = id : freeIds
            writeIORef ioState $ GlobalState freeIds' sessionsState'
            return rd
        rd@(ReturnData (Just matrix) _ _) -> do
            let Just (SessionState { aiXO }) = M.lookup id sessionsState
                sessionState' = SessionState matrix aiXO
                sessionsState' = M.insert id sessionState' sessionsState
            writeIORef ioState $ GlobalState freeIds sessionsState'
            return rd


server :: IOState -> Server API
server ioState = getId ioState
  :<|> move ioState

