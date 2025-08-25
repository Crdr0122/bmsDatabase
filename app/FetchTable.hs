{-# LANGUAGE OverloadedStrings #-}

module FetchTable where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

difficultyTables :: [(FilePath, Request)]
difficultyTables =
  [ ("Satellite", "https://stellabms.xyz/sl/score.json")
  , ("Solar", "https://stellabms.xyz/so/score.json")
  , ("Stella", "https://stellabms.xyz/st/score.json")
  , ("New_Generation_Normal", "https://rattoto10.github.io/second_table/score.json")
  , ("New_Generation_Insane", "https://rattoto10.github.io/second_table/insane_data.json")
  ]

getTables :: IO ()
getTables = mapM_ getTable difficultyTables

getTable :: (FilePath, Request) -> IO ()
getTable (n, url) = do
  response <- httpLBS url

  putStrLn $
    "The status code for "
      ++ n
      ++ " was: "
      ++ show (getResponseStatusCode response)
  L8.writeFile ("tables/" <> n <> ".json") $ getResponseBody response
