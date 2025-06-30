{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Database.SQLite.Simple

createTables :: Connection -> IO ()
createTables conn = do
  -- Songs represent logical groupings (one song may have multiple charts)
  execute_ conn "CREATE TABLE IF NOT EXISTS songs (id INTEGER PRIMARY KEY, title TEXT NOT NULL, artist TEXT NOT NULL, genre TEXT, UNIQUE(title, artist))"

  -- Folders containing one or more BMS files for a song
  execute_ conn "CREATE TABLE IF NOT EXISTS folders (id INTEGER PRIMARY KEY, path TEXT UNIQUE NOT NULL, song_id INTEGER NOT NULL, custom_name TEXT,           FOREIGN KEY(song_id) REFERENCES songs(id))"

  -- Individual BMS files
  execute_ conn "CREATE TABLE IF NOT EXISTS bms_files (id INTEGER PRIMARY KEY, filename TEXT NOT NULL, folder_id INTEGER NOT NULL, sha256 TEXT UNIQUE, md5 TEXT UNIQUE, play_length INTEGER, FOREIGN KEY(folder_id) REFERENCES folders(id))"

  -- Difficulty information from tables
  execute_ conn "CREATE TABLE IF NOT EXISTS difficulty_info ( id INTEGER PRIMARY KEY, song_id INTEGER NOT NULL, source_table TEXT NOT NULL, level TEXT, difficulty INTEGER, FOREIGN KEY(song_id) REFERENCES songs(id), UNIQUE(song_id, source_table))"
