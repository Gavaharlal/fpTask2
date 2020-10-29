module Main where

import Server
import System.Environment (getArgs)
import Text.Read
import Config

maybePort :: [String] -> Maybe Int
maybePort [] = pure defaultPort
maybePort (a : _) = readMaybe a

main :: IO ()
main = do
  args <- getArgs
  let x = maybePort args
  case x of
    Nothing -> putStrLn "Incorrect port specified"
    Just p -> startApp p