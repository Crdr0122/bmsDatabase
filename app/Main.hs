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
  configByteString <- getXdgDirectory XdgConfig "bmsDatabase/config.json" >>= BL.readFile
  (bmsFiles, t) <- case eitherDecode configByteString of
    Left err -> do
      putStrLn $ "Error parsing config file: " <> err
      error "Wrong Config"
    Right ConfigFile{configFileTables, actualBMSData} -> do
      let t = map (\DifficultyTable{tableName, tableUrl} -> (unpack tableName, parseRequest_ (unpack tableUrl))) configFileTables
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
