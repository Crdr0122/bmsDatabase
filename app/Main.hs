{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Schema

readBMSRecords :: FilePath -> IO (Either String [BMSRecord])
readBMSRecords filePath = do
  jsonData <- B.readFile filePath
  return $ eitherDecode jsonData

processJsonFile :: Connection -> FilePath -> IO ()
processJsonFile conn filePath = do
  result <- readBMSRecords filePath
  case result of
    Left err -> putStrLn $ "Error parsing JSON file " ++ filePath ++ ": " ++ err
    Right records -> do
      insertRecords conn filePath records
      putStrLn $ "Inserted " ++ show (length records) ++ " records from " ++ filePath

processJsonFiles :: [FilePath] -> IO ()
processJsonFiles filePaths = do
  conn <- open "bms.db"
  createTables conn
  mapM_ (processJsonFile conn) filePaths
  close conn
  putStrLn $ "Processed " ++ show (length filePaths) ++ " JSON files."

main :: IO ()
main = do
  let jsonFiles = ["tables/bms_second_normal.json", "tables/satellite.json"] -- Replace with your JSON file names
  processJsonFiles jsonFiles
