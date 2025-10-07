{-# LANGUAGE OverloadedStrings #-}

module FetchTable where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Schema (tablesFolder)

difficultyTables :: [(FilePath, Request)]
difficultyTables =
  [ ("Satellite", "https://stellabms.xyz/sl/score.json")
  , ("Solar", "https://stellabms.xyz/so/score.json")
  , ("New_Generation_Normal", "https://rattoto10.github.io/second_table/score.json")
  , ("New_Generation_Insane", "https://rattoto10.github.io/second_table/insane_data.json")
  , ("Starlight", "https://djkuroakari.github.io/data.json")
  , ("Normal", "https://darksabun.github.io/table/archive/normal1/data.json")
  , ("Insane", "https://darksabun.github.io/table/archive/insane1/data.json")
  , ("Scramble", "https://script.google.com/macros/s/AKfycbw5pnMwlCFZz7wDY5kRsBpfSm0-luKszs8LQAEE6BKkVT1R78-CpB4WA9chW-gdBsF7IA/exec")
  ]

getTables :: IO ()
getTables = mapM_ getTable difficultyTables

getTable :: (FilePath, Request) -> IO ()
getTable (n, url) = do
  response <- httpLBS url
  case getResponseStatusCode response of
    200 -> do
      L8.writeFile (tablesFolder <> n <> ".json") $ getResponseBody response
      putStrLn ("Updated " <> n)
    err -> putStrLn $ "Error for " ++ n ++ ": " ++ show err
