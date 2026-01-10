{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AppLogic where

import BMSFile (addBMSFiles, deleteBMSEntries, rebuildBMSFiles, renameBMSFolders)
import Control.Concurrent.Chan (Chan)
import Database.SQLite.Simple
import FetchTable (getTables, processTables)
import PrettyPrint (showMissing)
import Schema

rebuildDatabase :: Config -> LogChan -> IO ()
rebuildDatabase Config {..} logChan = do
  writeLog logChan "Starting database rebuild..."
  conn <- open dbPath
  execute_ conn "DROP TABLE IF EXISTS bms_files"
  createFileTable conn
  rebuildBMSFiles bmsFolder conn
  close conn
  writeLog logChan "Database rebuilt successfully"

addNewSongs :: Config -> LogChan -> IO ()
addNewSongs Config {..} logChan = do
  writeLog logChan "Adding new songs..."
  conn <- open dbPath
  addBMSFiles bmsFolder conn logChan
  close conn
  writeLog logChan "New songs added successfully"

cleanDatabase :: Config -> LogChan -> IO ()
cleanDatabase Config {..} logChan = do
  writeLog logChan "Cleaning database..."
  conn <- open dbPath
  deleteBMSEntries conn
  close conn
  writeLog logChan "Database cleaned successfully"

renameUncategorized :: Config -> LogChan -> IO ()
renameUncategorized Config {..} logChan = do
  writeLog logChan "Renaming uncategorized folders..."
  renameBMSFolders (bmsFolder <> "Uncategorized/")
  writeLog logChan "Uncategorized songs renamed successfully"

showMissingFiles :: Config -> LogChan -> IO ()
showMissingFiles Config {..} logChan = do
  writeLog logChan "Finding missing files..."
  conn <- open dbPath
  showMissing conn missingFiles
  close conn
  writeLog logChan $ "Missing files written to " ++ missingFiles

fetchTables :: Config -> LogChan -> IO ()
fetchTables Config {..} logChan = do
  writeLog logChan "Fetching tables..."
  getTables difficultyTables tablesFolder logChan
  writeLog logChan "Tables fetched successfully"

loadTables :: Config -> LogChan -> IO ()
loadTables Config {..} logChan = do
  writeLog logChan "Loading tables into database..."
  conn <- open dbPath
  let jsonFiles = (<> ".json") . (tablesFolder <>) . fst <$> difficultyTables
  processTables conn tablesFolder jsonFiles logChan
  close conn
  writeLog logChan "Tables loaded successfully"

-- refreshDirectory :: Config -> FilePath -> IO Text
-- refreshDirectory Config {..} songDirectory = do
--   conn <- open dbPath
--   entries <- listDirectory songDirectory
--   let fullPaths = map (songDirectory </>) entries
--       bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
--   mapM_ (processBMSFileIfExist conn) bmsFiles
--   close conn
--   return $ pack $ "Refreshed directory: " ++ songDirectory
