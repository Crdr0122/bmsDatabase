{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BMSFile (addBMSFiles, deleteBMSEntries, processBMSFileIfExist, rebuildBMSFiles, renameBMSFolders)
import Control.Monad (when)
import Control.Monad.Reader (asks, lift, runReaderT)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf)
import Data.Text (Text, unpack)
import Database.SQLite.Simple
import FetchTable (getTables, processTables)
import Network.HTTP.Simple
import PrettyPrint (showMissing)
import Schema
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), getXdgDirectory, listDirectory)
import System.FilePath (takeExtension, (</>))

data ConfigFile = ConfigFile
  { actualBMSData :: Text,
    configFileTables :: [DifficultyTable]
  }
  deriving (Show)

data DifficultyTable = DifficultyTable
  { tableUrl :: Text,
    tableName :: Text
  }
  deriving (Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \v ->
    ConfigFile
      <$> v .: "BMS Folder"
      <*> v .: "Difficulty Tables"

instance FromJSON DifficultyTable where
  parseJSON = withObject "DifficultyTable" $ \v ->
    DifficultyTable
      <$> v .: "url"
      <*> v .: "name"

processCommand :: String -> App Bool
processCommand input = do
  let args = words input
  case args of
    [] -> return True
    ["rebuild"] -> do
      db <- asks dbPath
      dataDir <- asks bmsFolder
      lift $ do
        conn <- open db
        execute_ conn "DROP TABLE IF EXISTS bms_files"
        createFileTable conn
        rebuildBMSFiles dataDir conn
        close conn
        putStrLn "Rebuilt Database"
      return True
    ["add"] -> do
      db <- asks dbPath
      dataDir <- asks bmsFolder
      lift $ do
        conn <- open db
        addBMSFiles dataDir conn
        close conn
        putStrLn "Added New Songs"
      return True
    ["fetch"] -> do
      tableFolder <- asks tablesFolder
      difficultyTableList <- asks difficultyTables
      lift $ do
        putStrLn "Fetching Tables"
        getTables difficultyTableList tableFolder
        putStrLn "Fetched Tables"
      return True
    ["clean"] -> do
      db <- asks dbPath
      lift $ do
        conn <- open db
        deleteBMSEntries conn
        putStrLn "Deleted Extra Entries"
      return True
    ["load"] -> do
      db <- asks dbPath
      dT <- asks difficultyTables
      tF <- asks tablesFolder
      lift $ do
        conn <- open db
        let jsonFiles = (<> ".json") . (tF <>) . fst <$> dT
        processTables conn tF jsonFiles
        putStrLn "Added All Tables"
      return True
    ["rename"] -> do
      actualData <- asks bmsFolder
      lift $ do
        renameBMSFolders (actualData <> "Uncategorized/")
        putStrLn "Renamed Songs in Uncategorized"
      return True
    ["refresh ", songDirectory] -> do
      db <- asks dbPath
      lift $ do
        conn <- open db
        entries <- listDirectory songDirectory
        let fullPaths = map (songDirectory </>) entries
            bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
        mapM_ (processBMSFileIfExist conn) bmsFiles
        putStrLn "Inserted One Song"
        close conn
      return True
    ["show"] -> do
      db <- asks dbPath
      missing <- asks missingFiles
      lift $ do
        conn <- open db
        putStrLn "Writing Missing Files"
        showMissing conn missing
      return True
    ["quit"] -> return False
    _ -> do
      lift $ do
        putStrLn $ "Unknown command or invalid arguments: " ++ input
        putStrLn "Valid commands: add (bms files), clean, refresh <dir>, fetch (tables), show (missing), load (tables), rename (uncategorized), rebuild (database), quit"
      return True

main :: IO ()
main = do
  configFile <- getXdgDirectory XdgConfig "bmsDatabase/config.json"
  configByteString <- BL.readFile configFile
  (bmsFiles, t) <- case eitherDecode configByteString of
    Left err -> do
      putStrLn $ "Error parsing config file: " ++ err
      return ("", [])
    Right ConfigFile {actualBMSData = bmsF, configFileTables = f} -> do
      let t = map (\DifficultyTable {tableName = x, tableUrl = y} -> (unpack x, parseRequest_ (unpack y))) f
      return (unpack bmsF, t)

  xdgDataDir <- getXdgDirectory XdgData "bmsDatabase/"
  let config =
        Config
          { bmsFolder = bmsFiles,
            dbPath = xdgDataDir <> "bms.db",
            missingFiles = xdgDataDir <> "missing.md",
            tablesFolder = xdgDataDir <> "tables/",
            difficultyTables = t
          }
  runReaderT (runInputT (setComplete completeFunc defaultSettings) loop) config
  where
    loop :: InputT App ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> do
          continue <- lift $ processCommand input
          when continue loop

completeFunc :: CompletionFunc App
completeFunc (left, _) = do
  let input = reverse left -- Haskeline provides reversed input
      candidates = filter (input `isPrefixOf`) validCommands
  return ("", [Completion c c False | c <- candidates])
  where
    validCommands = ["add", "clean", "load", "fetch", "show", "refresh", "rename", "rebuild", "quit"]
