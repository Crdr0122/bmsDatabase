{-# LANGUAGE OverloadedStrings #-}

module FetchTable where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Database.SQLite.Simple
import Network.HTTP.Simple
import Schema (App, BMSRecord, createRecordTable, insertRecord)

-- difficultyTables :: [(FilePath, Request)]
-- difficultyTables =
--   [ ("Satellite", "https://stellabms.xyz/sl/score.json"),
--     ("Solar", "https://stellabms.xyz/so/score.json"),
--     ("New_Generation_Normal", "https://rattoto10.github.io/second_table/score.json"),
--     ("New_Generation_Insane", "https://rattoto10.github.io/second_table/insane_data.json"),
--     ("Starlight", "https://djkuroakari.github.io/data.json"),
--     ("Normal", "https://darksabun.github.io/table/archive/normal1/data.json"),
--     ("Insane", "https://darksabun.github.io/table/archive/insane1/data.json"),
--     ("Scramble", "https://script.google.com/macros/s/AKfycbw5pnMwlCFZz7wDY5kRsBpfSm0-luKszs8LQAEE6BKkVT1R78-CpB4WA9chW-gdBsF7IA/exec")
--   ]
--
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

readBMSRecords :: FilePath -> IO (Either String [BMSRecord])
readBMSRecords f = do
  jsonData <- B.readFile f
  return $ eitherDecode jsonData

processTable :: Connection -> FilePath -> IO ()
processTable conn f = do
  result <- readBMSRecords f
  let tableName = takeWhile (/= '.') $ drop (length tablesFolder) f
  case result of
    Left err -> putStrLn $ "Error parsing JSON file " ++ f ++ ": " ++ err
    Right records -> do
      mapM_ (insertRecord conn tableName) records
      putStrLn $ "Inserted " ++ show (length records) ++ " records from " ++ tableName

processTables :: [FilePath] -> IO ()
processTables filePaths = do
  conn <- open bmsDatabase
  execute_ conn "DROP TABLE IF EXISTS bms_records"
  createRecordTable conn
  mapM_ (processTable conn) filePaths
  close conn
  putStrLn $ "Processed " ++ show (length filePaths) ++ " tables."
