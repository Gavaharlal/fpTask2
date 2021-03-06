module Requests (createSessionAPI, makeMoveAPI) where

import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as H
import Data.Text (Text, pack,  unpack)

import Data.Char (isDigit)
import Network.HTTP.Req
import Data.Aeson hiding (Error)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import Prelude hiding (id, error)

import Algorithm

import Data.Aeson.TH
$(deriveJSON defaultOptions ''XO)

createSessionAPI :: MonadIO m => Int -> String -> Int -> m (Either String ([[XO]], Int, XO))
createSessionAPI size address portParam = runReq defaultHttpConfig $ do
  r <-
    req
      GET
      (http (pack address) /: "session-create")
      NoReqBody
      jsonResponse $
      ("size" =: (size :: Int)) <>
      port portParam
  let requestData = (responseBody r :: Object)
      Success newMatrix = fromJSON (fromJust $ H.lookup "matrix" requestData) :: Result [[XO]]
      Success id = fromJSON (fromJust $ H.lookup "id" requestData) :: Result Int
      Success humanXO = fromJSON (fromJust $ H.lookup "humanXO" requestData) :: Result XO
      error = fromJSON (fromJust $ H.lookup "error" requestData) :: Result String
      result = case error of
          Success errorMsg -> Left errorMsg
          _                -> Right (newMatrix, id, humanXO)
  return result

makeMoveAPI :: MonadIO m => Int -> Int -> Int -> String -> Int -> m (Either String ([[XO]], Maybe XO))
makeMoveAPI id x y address portParam = runReq defaultHttpConfig $ do
  r <-
    req
      GET
      (http (pack address) /: "move")
      NoReqBody
      jsonResponse $
      ("id" =: (id :: Int)) <>
      ("x" =: (x :: Int)) <>
      ("y" =: (y :: Int)) <>
      port portParam
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

