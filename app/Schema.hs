{-# LANGUAGE RecordWildCards #-}

module Schema (
  BMSRecord (..),
  BMSFile (..),
  Config (..),
  ConfigFile (..),
  DifficultyTable (..),
  LogMessage (..),
  LogChan,
  validExts,
  normalizeTitle,
  insertBMSFile,
  insertRecord,
  createRecordTable,
  writeLog,
  createFileTable,
  threadUpdateMain,
)
where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad (void)
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import GI.GLib qualified as GLib
import Network.HTTP.Simple

data LogMessage = LogMessage T.Text | ClearLog

type LogChan = Chan LogMessage

writeLog :: LogChan -> String -> IO ()
writeLog chan = writeChan chan . LogMessage . T.pack

data Config = Config
  { bmsFolder :: FilePath
  , dbPath :: FilePath
  , tablesFolder :: FilePath
  , difficultyTables :: [(FilePath, Request)]
  }
  deriving (Show)

data BMSRecord = BMSRecord
  { artist :: Text
  , level :: Text
  , title :: Text
  , url :: Maybe Text
  , url_diff :: Maybe Text
  , comment :: Maybe Text
  , md5 :: Maybe Text
  , sha256 :: Maybe Text
  }
  deriving (Show)

data BMSFile = BMSFile
  { fArtist :: Text
  , fTitle :: Text
  , fMd5 :: Maybe Text
  , fSha256 :: Maybe Text
  , filePath :: Text
  }
  deriving (Show)

instance FromJSON BMSRecord where
  parseJSON = withObject "BMSRecord" $ \v ->
    BMSRecord
      <$> v .: "artist"
      <*> v .: "level"
      <*> v .: "title"
      <*> v .:? "url"
      <*> v .:? "url_diff"
      <*> v .:? "comment"
      <*> v .:? "md5"
      <*> v .:? "sha256"

data ConfigFile = ConfigFile
  { actualBMSData :: Text
  , configFileTables :: [DifficultyTable]
  }
  deriving (Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \v ->
    ConfigFile
      <$> v .: "BMS Folder"
      <*> v .: "Difficulty Tables"

data DifficultyTable = DifficultyTable
  { tableUrl :: Text
  , tableName :: Text
  }
  deriving (Show)

instance FromJSON DifficultyTable where
  parseJSON = withObject "DifficultyTable" $ \v ->
    DifficultyTable
      <$> v .: "url"
      <*> v .: "name"

validExts :: [String]
validExts = [".bms", ".bme", ".bmson", ".bml", ".pms", ".BME", ".BMS", ".BML", ".BMSON", ".PMS"]

createRecordTable :: Connection -> IO ()
createRecordTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS bms_records (id INTEGER PRIMARY KEY AUTOINCREMENT, source_table TEXT NOT NULL, artist TEXT, title TEXT, level TEXT, url TEXT, url_diff TEXT, comment TEXT, md5 TEXT, sha256 TEXT, UNIQUE(title, md5, sha256, source_table))"

createFileTable :: Connection -> IO ()
createFileTable conn =
  execute_ conn "CREATE TABLE IF NOT EXISTS bms_files (id INTEGER PRIMARY KEY AUTOINCREMENT, artist TEXT, title TEXT, file_path TEXT NOT NULL, md5 TEXT, sha256 TEXT, UNIQUE(file_path))"

insertRecord :: Connection -> String -> BMSRecord -> IO ()
insertRecord conn sourceTable BMSRecord{..} =
  execute
    conn
    "INSERT INTO bms_records (source_table, artist, level, title, url, url_diff, comment, md5, sha256) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    (sourceTable, artist, level, title, url, url_diff, comment, md5, sha256)

insertBMSFile :: Connection -> FilePath -> BMSFile -> IO ()
insertBMSFile conn fp BMSFile{..} =
  execute
    conn
    "INSERT INTO bms_files (file_path, artist, title, md5, sha256) VALUES (?,?,?,?,?)"
    (fp, fArtist, fTitle, fMd5, fSha256)

commonPrefix :: [Text] -> Text
commonPrefix [] = ""
commonPrefix (x : xs) = foldl' pre x xs
 where
  pre a b = case T.commonPrefixes a b of
    Nothing -> ""
    Just (p, _, _) -> p

normalizeTitle :: [Text] -> String
normalizeTitle = T.unpack . flip (foldl' (\n (from, to) -> T.replace from to n)) illegalCharacters . T.strip . commonPrefix
 where
  illegalCharacters =
    [ ("/", "／")
    , (":", "：")
    , ("?", "？")
    , ("\\", "＼")
    , ("*", "＊")
    , ("<", "＜")
    , (">", "＞")
    , ("|", "｜")
    , ("\"", "＂")
    ]

threadUpdateMain :: IO () -> IO ()
threadUpdateMain action =
  void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
    action
    return GLib.SOURCE_REMOVE
