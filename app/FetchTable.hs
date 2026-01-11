{-# LANGUAGE OverloadedStrings #-}

module FetchTable
  ( getTables,
    processTables,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Database.SQLite.Simple
import Network.HTTP.Simple
import Schema (BMSRecord, LogChan, createRecordTable, insertRecord, writeLog)

getTables :: [(FilePath, Request)] -> FilePath -> LogChan -> IO ()
getTables difficultyTableList tableFolder logChan = mapM_ (getTable tableFolder logChan) difficultyTableList

getTable :: FilePath -> LogChan -> (FilePath, Request) -> IO ()
getTable tableFolder logChan (n, url) = do
  response <- httpLBS url
  case getResponseStatusCode response of
    200 -> do
      L8.writeFile (tableFolder <> n <> ".json") $ getResponseBody response
      -- putStrLn ("Updated " <> n)
      writeLog logChan ("Updated " <> n)
    err -> writeLog logChan $ "Error for " ++ n ++ ": " ++ show err

readBMSRecords :: FilePath -> IO (Either String [BMSRecord])
readBMSRecords f = do
  jsonData <- B.readFile f
  return $ eitherDecode jsonData

processTables :: Connection -> FilePath -> [FilePath] -> LogChan -> IO ()
processTables conn tableFolder filePaths logChan = do
  execute_ conn "DROP TABLE IF EXISTS bms_records"
  createRecordTable conn
  mapM_ (processTable conn tableFolder logChan) filePaths
  close conn
  writeLog logChan $ "Processed " ++ show (length filePaths) ++ " tables."

processTable :: Connection -> FilePath -> LogChan -> FilePath -> IO ()
processTable conn tableFolder logChan tableFile = do
  result <- readBMSRecords tableFile
  let tableName = takeWhile (/= '.') $ drop (length tableFolder) tableFile
  case result of
    Left err -> writeLog logChan $ "Error parsing JSON file " ++ tableFile ++ ": " ++ err
    Right records -> do
      mapM_ (insertRecord conn tableName) records
      writeLog logChan $ "Inserted " ++ show (length records) ++ " records from " ++ tableName
