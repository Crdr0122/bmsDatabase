{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import FetchTable (difficultyTables, getTables)
import Schema
import BMSFile (processBMS)

readBMSRecords :: FilePath -> IO (Either String [BMSRecord])
readBMSRecords filePath = do
  jsonData <- B.readFile filePath
  return $ eitherDecode jsonData

processJsonFile :: Connection -> FilePath -> IO ()
processJsonFile conn filePath = do
  result <- readBMSRecords filePath
  let tableName = drop 7 $ takeWhile (/= '.') filePath
  case result of
    Left err -> putStrLn $ "Error parsing JSON file " ++ filePath ++ ": " ++ err
    Right records -> do
      mapM_ (insertRecord conn tableName) records
      putStrLn $ "Inserted " ++ show (length records) ++ " records from " ++ tableName

processJsonFiles :: [FilePath] -> IO ()
processJsonFiles filePaths = do
  conn <- open "bms.db"
  execute_ conn "DROP TABLE IF EXISTS bms_records"
  createTables conn
  mapM_ (processJsonFile conn) filePaths
  close conn
  putStrLn $ "Processed " ++ show (length filePaths) ++ " JSON files."

main :: IO ()
main = do
  arg <- getLine
  case arg of
    "t" -> do
      b <- processBMS "t.bms"
      print b 
    "Tables" -> do
      putStrLn "Fetching Tables"
      getTables
    "Database" -> do
      let jsonFiles = (<> ".json") . ("tables/" <>) . fst <$> difficultyTables -- Replace with your JSON file names
      processJsonFiles jsonFiles
    (stripPrefix "Database" -> Just tableName) -> do return ()
    _ -> return ()
  main
