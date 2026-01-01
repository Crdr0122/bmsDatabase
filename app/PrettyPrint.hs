{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import Data.Text (Text, unpack)
import Database.SQLite.Simple

showMissing :: Connection -> FilePath -> IO ()
showMissing conn missing = do
  res <-
    query_
      conn
      "SELECT source_table, title, artist, level, url, url_diff FROM bms_records WHERE NOT EXISTS (SELECT 1 FROM bms_files WHERE bms_records.md5 = bms_files.md5 OR bms_records.sha256 = bms_files.sha256)"
  close conn
  putStrLn "Missing files:"
  mapM_ print (res :: [(Text, Text, Text, Text, Maybe Text, Maybe Text)])
  let output =
        if null res
          then "# Missing Files\n\nNone\n"
          else "# Missing Files\n\n" ++ concatMap formatRecord res
      formatRecord (source_table, title, artist, level, url, url_diff) =
        let urlText = maybe "None" (\u -> "[" ++ unpack u ++ "](" ++ unpack u ++ ")") url
            urlDiffText = maybe "None" (\u -> "[" ++ unpack u ++ "](" ++ unpack u ++ ")") url_diff
         in "## "
              ++ unpack source_table
              ++ "\n"
              ++ "- **Title**: "
              ++ unpack title
              ++ "\n"
              ++ "- **Artist**: "
              ++ unpack artist
              ++ "\n"
              ++ "- **Level**: "
              ++ unpack level
              ++ "\n"
              ++ "- **URL**: "
              ++ urlText
              ++ "\n"
              ++ "- **URL Diff**: "
              ++ urlDiffText
              ++ "\n\n"
  writeFile missing output
  putStrLn "Missing files written to missing_files.md"
