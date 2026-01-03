{-# LANGUAGE OverloadedStrings #-}

module FetchTable where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Database.SQLite.Simple
import Network.HTTP.Simple
import Schema (BMSRecord, createRecordTable, insertRecord)

getTables :: [(FilePath, Request)] -> FilePath -> IO ()
getTables difficultyTableList tableFolder = mapM_ (getTable tableFolder) difficultyTableList

getTable :: FilePath -> (FilePath, Request) -> IO ()
getTable tableFolder (n, url) = do
  response <- httpLBS url
  case getResponseStatusCode response of
    200 -> do
      L8.writeFile (tableFolder <> n <> ".json") $ getResponseBody response
      putStrLn ("Updated " <> n)
    err -> putStrLn $ "Error for " ++ n ++ ": " ++ show err

readBMSRecords :: FilePath -> IO (Either String [BMSRecord])
readBMSRecords f = do
  jsonData <- B.readFile f
  return $ eitherDecode jsonData

processTables :: Connection -> FilePath -> [FilePath] -> IO ()
processTables conn tableFolder filePaths = do
  execute_ conn "DROP TABLE IF EXISTS bms_records"
  createRecordTable conn
  mapM_ (processTable conn tableFolder) filePaths
  close conn
  putStrLn $ "Processed " ++ show (length filePaths) ++ " tables."

processTable :: Connection -> FilePath -> FilePath -> IO ()
processTable conn tableFolder tableFile = do
  result <- readBMSRecords tableFile
  let tableName = takeWhile (/= '.') $ drop (length tableFolder) tableFile
  case result of
    Left err -> putStrLn $ "Error parsing JSON file " ++ tableFile ++ ": " ++ err
    Right records -> do
      mapM_ (insertRecord conn tableName) records
      putStrLn $ "Inserted " ++ show (length records) ++ " records from " ++ tableName
