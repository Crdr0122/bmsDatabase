{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (unpack)
import GUI
import Network.HTTP.Simple
import Schema
import System.Directory (XdgDirectory (..), getXdgDirectory)

main :: IO ()
main = do
  configFile <- getXdgDirectory XdgConfig "bmsDatabase/config.json"
  configByteString <- BL.readFile configFile
  (bmsFiles, t) <- case eitherDecode configByteString of
    Left err -> do
      putStrLn $ "Error parsing config file: " ++ err
      error "Wrong Config"
    Right ConfigFile {..} -> do
      let t = map (\DifficultyTable {..} -> (unpack tableName, parseRequest_ (unpack tableUrl))) configFileTables
      return (unpack actualBMSData, t)

  xdgDataDir <- getXdgDirectory XdgData "bmsDatabase/"
  let config =
        Config
          { bmsFolder = bmsFiles,
            dbPath = xdgDataDir <> "bms.db",
            missingFiles = xdgDataDir <> "missing.md",
            tablesFolder = xdgDataDir <> "tables/",
            difficultyTables = t
          }
  GUI.startApp config
