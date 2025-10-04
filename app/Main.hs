{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import BMSFile (processBMS, processBMSON)
import Control.Monad (filterM)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import FetchTable (difficultyTables, getTables)
import Schema
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

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
  createRecordTable conn
  mapM_ (processJsonFile conn) filePaths
  close conn
  putStrLn $ "Processed " ++ show (length filePaths) ++ " JSON files."

processBMSFiles :: [FilePath] -> IO ()
processBMSFiles filepaths = do
  conn <- open "bms.db"
  execute_ conn "DROP TABLE IF EXISTS bms_files"
  createFileTable conn
  mapM_ (processBMSFile conn) filepaths
  close conn
  putStrLn $ "Processed " ++ show (length filepaths) ++ " BMS files."

processBMSFile :: Connection -> FilePath -> IO ()
processBMSFile conn f = do
  b <- if "bmson" `isSuffixOf` f then processBMSON f else processBMS f
  insertBMSFile conn f b

showMissing :: IO ()
showMissing = do
  conn <- open "bms.db"
  res <-
    query_
      conn
      "SELECT source_table, title, artist, level, url FROM bms_records WHERE NOT EXISTS (SELECT 1 FROM bms_files WHERE bms_records.md5 = bms_files.md5 OR bms_records.sha256 = bms_files.sha256)"
  close conn
  putStrLn "Missing files:"
  mapM_ print (res :: [(Text, Text, Text, Text, Maybe Text)])

getBMSFiles :: FilePath -> Connection -> IO ()
getBMSFiles rootDir conn = do
  packs <- getSubdirectories rootDir
  mapM_ getFilesFromSubdirs packs
 where
  getSubdirectories :: FilePath -> IO [FilePath]
  getSubdirectories dir = do
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
    filterM doesDirectoryExist fullPaths

  getFilesFromSubdirs :: FilePath -> IO ()
  getFilesFromSubdirs dir = do
    subdirs <- getSubdirectories dir
    mapM_ insertFiles subdirs

  insertFiles :: FilePath -> IO ()
  insertFiles dir = do
    putStrLn dir
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
        validExts = [".bms", ".bme", ".bmson", ".bml", ".pms", ".BME", ".BMS", ".BML", ".BMSON", ".PMS"]
        bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
    mapM_ (processBMSFile conn) bmsFiles

main :: IO ()
main = do
  arg <- getLine
  case arg of
    "f" -> do
      conn <- open "bms.db"
      execute_ conn "DROP TABLE IF EXISTS bms_files"
      createFileTable conn
      getBMSFiles "/mnt/Storage/BMS stuff/" conn
      close conn
    "r" -> do
      processBMSFiles ["t.bms", "t.bmson"]
    "t" -> do
      putStrLn "Fetching Tables"
      getTables
    "d" -> do
      let jsonFiles = (<> ".json") . ("tables/" <>) . fst <$> difficultyTables -- Replace with your JSON file names
      processJsonFiles jsonFiles
    (stripPrefix "d" -> Just tableName) -> do
      putStrLn tableName
    "s" -> showMissing
    _ -> return ()
  main
