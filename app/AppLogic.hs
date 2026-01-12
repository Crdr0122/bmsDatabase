{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AppLogic where

import BMSFile (addBMSFiles, deleteBMSEntries, rebuildBMSFiles, renameBMSFolders)
import Control.Monad (void)
import Database.SQLite.Simple
import FetchTable (getTables, processTables)
import qualified GI.GLib as GLib
import GI.Gio (ListStore, listStoreRemoveAll, listStoreSplice)
import PrettyPrint (showMissing)
import Schema
import TypeWrappers (toBMSFileWrapper)

rebuildDatabase :: ListStore -> Config -> LogChan -> IO ()
rebuildDatabase listStore c@Config{..} logChan = do
  writeLog logChan "Starting database rebuild..."
  conn <- open dbPath
  execute_ conn "DROP TABLE IF EXISTS bms_files"
  createFileTable conn
  rebuildBMSFiles bmsFolder conn
  writeLog logChan "Database rebuilt successfully"
  showAllFiles listStore c

addNewSongs :: ListStore -> Config -> LogChan -> IO ()
addNewSongs listStore c@Config{..} logChan = do
  writeLog logChan "Adding new songs..."
  conn <- open dbPath
  addBMSFiles bmsFolder conn logChan
  writeLog logChan "New songs added successfully"
  showAllFiles listStore c

cleanDatabase :: ListStore -> Config -> LogChan -> IO ()
cleanDatabase listStore c@Config{..} logChan = do
  writeLog logChan "Cleaning database..."
  conn <- open dbPath
  deleteBMSEntries conn
  writeLog logChan "Database cleaned successfully"
  showAllFiles listStore c

renameUncategorized :: Config -> LogChan -> IO ()
renameUncategorized Config{..} logChan = do
  writeLog logChan "Renaming uncategorized folders..."
  renameBMSFolders (bmsFolder <> "Uncategorized/")
  writeLog logChan "Uncategorized songs renamed successfully"

showMissingFiles :: ListStore -> Config -> LogChan -> IO ()
showMissingFiles listStore Config{..} logChan = do
  writeLog logChan "Finding missing files..."
  conn <- open dbPath
  showMissing conn listStore
  writeLog logChan "Missing files found"

fetchTables :: Config -> LogChan -> IO ()
fetchTables Config{..} logChan = do
  writeLog logChan "Fetching tables..."
  getTables difficultyTables tablesFolder logChan
  writeLog logChan "Tables fetched successfully"

loadTables :: Config -> LogChan -> IO ()
loadTables Config{..} logChan = do
  writeLog logChan "Loading tables into database..."
  conn <- open dbPath
  let jsonFiles = (<> ".json") . (tablesFolder <>) . fst <$> difficultyTables
  processTables conn tablesFolder jsonFiles logChan
  writeLog logChan "Tables loaded successfully"

showAllFiles :: ListStore -> Config -> IO ()
showAllFiles listStore Config{..} = do
  conn <- open dbPath
  createRecordTable conn
  res <- query_ conn "SELECT artist, title, file_path FROM bms_files"
  close conn
  let bmsFiles =
        ( \(a, t, f) ->
            BMSFile a t Nothing Nothing f
        )
          <$> res
  bmsFilesWrapped <- toBMSFileWrapper bmsFiles
  void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
    listStoreRemoveAll listStore
    listStoreSplice listStore 0 0 bmsFilesWrapped
    return False

-- refreshDirectory :: Config -> FilePath -> IO Text
-- refreshDirectory Config {..} songDirectory = do
--   conn <- open dbPath
--   entries <- listDirectory songDirectory
--   let fullPaths = map (songDirectory </>) entries
--       bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
--   mapM_ (processBMSFileIfExist conn) bmsFiles
--   close conn
--   return $ pack $ "Refreshed directory: " ++ songDirectory
