{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import BMSFile (processBMS,processBMSON)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import FetchTable (difficultyTables, getTables)
import Schema

readBMSRecords :: FilePath -> IO (Either String [BMSRecord])
readBMSRecords f = do
  jsonData <- B.readFile f
  return $ eitherDecode jsonData

processJsonFile :: Connection -> FilePath -> IO ()
processJsonFile conn f = do
  result <- readBMSRecords f
  let tableName = drop 7 $ takeWhile (/= '.') f
  case result of
    Left err -> putStrLn $ "Error parsing JSON file " ++ f ++ ": " ++ err
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
    "r" -> do
      b <- processBMS "t.bms"
      bon <- processBMSON "t.bmson"
      print b
      print bon
    "t" -> do
      putStrLn "Fetching Tables"
      getTables
    "d" -> do
      let jsonFiles = (<> ".json") . ("tables/" <>) . fst <$> difficultyTables -- Replace with your JSON file names
      processJsonFiles jsonFiles
    (stripPrefix "d" -> Just tableName) -> do return ()
    _ -> return ()
  main
