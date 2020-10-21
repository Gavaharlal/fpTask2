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
import Algorithm

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data Admin = Admin
  { adminId        :: Int
  , adminName      :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Admin)

type API = "new-session-create" :> Get '[JSON] Int
    :<|> "session" :> Capture "id" Int :> "size" :> Capture "n" Int :> Get '[JSON] String

type MatrixState = IORef [[[XO]]]

startApp :: IO ()
startApp = run 8080 =<< app

-- app :: Application
app = (serve api . server) <$> newIORef []

api :: Proxy API
api = Proxy

getId :: MatrixState -> Handler Int
getId matrixState = liftIO $ do
    matrixList <- readIORef matrixState
    let updatedMList = matrixList ++ [createBlankMatrix 3]
    writeIORef matrixState updatedMList
    return $ length matrixList

server :: MatrixState -> Server API
server matrixState = getId matrixState
  :<|> (\i n-> (return $ show i ++ " " ++ show n))

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

admin :: Admin
admin = Admin 1 "Lol"

