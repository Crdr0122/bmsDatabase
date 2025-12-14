{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BMSFile (addBMSFiles, deleteBMSEntries, processBMSFileIfExist, rebuildBMSFiles, renameBMSFolders)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Database.SQLite.Simple
import FetchTable (difficultyTables, getTables, processTables)
import PrettyPrint (showMissing)
import Schema
import System.Console.Haskeline
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

processCommand :: String -> IO Bool
processCommand input = do
  let args = words input
  case args of
    [] -> return True
    ["rebuild"] -> do
      conn <- open bmsDatabase
      execute_ conn "DROP TABLE IF EXISTS bms_files"
      createFileTable conn
      rebuildBMSFiles bmsActualData conn
      close conn
      putStrLn "Rebuilt Database"
      return True
    ["add"] -> do
      conn <- open bmsDatabase
      addBMSFiles bmsActualData conn
      close conn
      putStrLn "Added New Songs"
      return True
    ["fetch"] -> do
      putStrLn "Fetching Tables"
      getTables
      putStrLn "Fetched Tables"
      return True
    ["clean"] -> do
      deleteBMSEntries
      putStrLn "Deleted Extra Entries"
      return True
    ["load"] -> do
      let jsonFiles = (<> ".json") . (tablesFolder <>) . fst <$> difficultyTables
      processTables jsonFiles
      putStrLn "Added All Tables"
      return True
    ["rename"] -> do
      renameBMSFolders (bmsActualData <> "Uncategorized/")
      putStrLn "Renamed Songs in Uncategorized"
      return True
    ["refresh ", songDirectory] -> do
      conn <- open bmsDatabase
      entries <- listDirectory songDirectory
      let fullPaths = map (songDirectory </>) entries
          bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
      mapM_ (processBMSFileIfExist conn) bmsFiles
      putStrLn "Inserted One Song"
      close conn
      return True
    ["show"] -> do
      putStrLn "Writing Missing Files"
      showMissing
      return True
    ["quit"] -> return False
    _ -> do
      putStrLn $ "Unknown command or invalid arguments: " ++ input
      putStrLn "Valid commands: add (bms files), clean, refresh <dir>, fetch (tables), show (missing), load (tables), rename (uncategorized), rebuild (database), quit"
      return True

main :: IO ()
main =
  runInputT (setComplete completeFunc defaultSettings) loop
 where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        continue <- liftIO $ processCommand input
        when continue loop

completeFunc :: CompletionFunc IO
completeFunc (left, _) = do
  let input = reverse left -- Haskeline provides reversed input
      candidates = filter (input `isPrefixOf`) validCommands
  return ("", [Completion c c False | c <- candidates])
 where
  validCommands = ["add", "clean", "load", "fetch", "show", "refresh", "rename", "rebuild", "quit"]
