{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BMSFile (addBMSFiles, deleteBMSEntries, processBMSFileIfExist, rebuildBMSFiles, renameBMSFolders)
import Control.Monad (when)
import Control.Monad.Reader -- for ReaderT, ask, asks, runReaderT
import Data.List (isPrefixOf)
import Database.SQLite.Simple
import FetchTable (getTables, processTables)
import PrettyPrint (showMissing)
import Schema
import System.Console.Haskeline
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

processCommand :: String -> App Bool
processCommand input = do
  let args = words input
  case args of
    [] -> return True
    ["rebuild"] -> do
      db <- asks dbPath
      dataDir <- asks bmsFolder
      liftIO $ do
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
      liftIO $ do
        conn <- open db
        addBMSFiles dataDir conn
        close conn
        putStrLn "Added New Songs"
      return True
    ["fetch"] -> do
      -- TODO
      liftIO $ do
        putStrLn "Fetching Tables"
        getTables
        putStrLn "Fetched Tables"
      return True
    ["clean"] -> do
      db <- asks dbPath
      liftIO $ do
        conn <- open db
        deleteBMSEntries conn
        putStrLn "Deleted Extra Entries"
      return True
    ["load"] -> do
      dT <- asks difficultyTables
      tF <- asks tablesFolder
      liftIO $ do
        let jsonFiles = (<> ".json") . (tF <>) . fst <$> dT
        processTables jsonFiles
        putStrLn "Added All Tables"
      return True
    ["rename"] -> do
      actualData <- asks bmsFolder
      liftIO $ do
        renameBMSFolders (actualData <> "Uncategorized/")
        putStrLn "Renamed Songs in Uncategorized"
      return True
    ["refresh ", songDirectory] -> do
      db <- asks dbPath
      liftIO $ do
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
      liftIO $ do
        conn <- open db
        putStrLn "Writing Missing Files"
        showMissing conn missing
      return True
    ["quit"] -> return False
    _ -> do
      liftIO $ do
        putStrLn $ "Unknown command or invalid arguments: " ++ input
        putStrLn "Valid commands: add (bms files), clean, refresh <dir>, fetch (tables), show (missing), load (tables), rename (uncategorized), rebuild (database), quit"
      return True

main :: IO ()
main = do
  let dataFolder = "/home/yu/.local/share/bmsDatabase/"
      config =
        Config
          { bmsFolder = "/mnt/Storage/BMS stuff/",
            dbPath = dataFolder <> "bms.db",
            missingFiles = dataFolder <>"missing.md",
            tablesFolder = dataFolder <> "tables/",
            difficultyTables = []
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
