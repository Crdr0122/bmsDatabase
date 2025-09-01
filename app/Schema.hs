{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import Database.SQLite.Simple

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
  , fSha256 :: Maybe BS.ByteString
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

createTables :: Connection -> IO ()
createTables conn = do
  -- Difficulty information from tables
  execute_ conn "CREATE TABLE IF NOT EXISTS bms_records (id INTEGER PRIMARY KEY AUTOINCREMENT, source_table TEXT NOT NULL, artist TEXT, title TEXT, level TEXT, url TEXT, url_diff TEXT, comment TEXT, md5 TEXT, sha256 TEXT, UNIQUE(title, md5, sha256, source_table))"

insertRecord :: Connection -> String -> BMSRecord -> IO ()
insertRecord conn sourceTable record =
  execute
    conn
    "INSERT INTO bms_records (source_table, artist, level, title, url, url_diff, comment, md5, sha256) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    (sourceTable, artist record, level record, title record, url record, url_diff record, comment record, md5 record, sha256 record)
