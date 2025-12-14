{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Schema where

import Data.Aeson
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple

dataFolder :: String
dataFolder = "/home/yu/.local/share/bmsDatabase/"
tablesFolder :: String
tablesFolder = dataFolder <> "tables/"
bmsDatabase :: String
bmsDatabase = dataFolder <> "bms.db"
bmsActualData :: String
bmsActualData = "/mnt/Storage/BMS stuff/"

validExts :: [String]
validExts = [".bms", ".bme", ".bmson", ".bml", ".pms", ".BME", ".BMS", ".BML", ".BMSON", ".PMS"]

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

insertBMSFile :: Connection -> String -> BMSFile -> IO ()
insertBMSFile conn fp BMSFile{..} =
  execute
    conn
    "INSERT INTO bms_files (file_path, artist, title, md5, sha256) VALUES (?,?,?,?,?)"
    (fp, fArtist, fTitle, fMd5, fSha256)

commonPrefix :: [Text] -> Text
commonPrefix = foldl1 pre
 where
  pre x y = case T.commonPrefixes x y of
    Nothing -> ""
    Just (p, _, _) -> p

normalizeTitle :: [Text] -> String
normalizeTitle x = T.unpack $ foldl' (\n (from, to) -> T.replace from to n) (T.strip $ commonPrefix x) illegalCharacters
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
