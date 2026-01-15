{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (unpack)
import GUI (startApp)
import Network.HTTP.Simple (parseRequest_)
import Schema (Config (..), ConfigFile (..), DifficultyTable (..))
import System.Directory (XdgDirectory (..), getXdgDirectory)

main :: IO ()
main = do
  configFile <- getXdgDirectory XdgConfig "bmsDatabase/config.json"
  configByteString <- BL.readFile configFile
  (bmsFiles, t) <- case eitherDecode configByteString of
    Left err -> do
      putStrLn $ "Error parsing config file: " ++ err
      error "Wrong Config"
    Right ConfigFile{..} -> do
      let t = map (\DifficultyTable{..} -> (unpack tableName, parseRequest_ (unpack tableUrl))) configFileTables
      return (unpack actualBMSData, t)

  xdgDataDir <- getXdgDirectory XdgData "bmsDatabase/"
  let config =
        Config
          { bmsFolder = bmsFiles
          , dbPath = xdgDataDir <> "bms.db"
          , tablesFolder = xdgDataDir <> "tables/"
          , difficultyTables = t
          }
  startApp config
