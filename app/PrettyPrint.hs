module PrettyPrint (showMissing) where

import Database.SQLite.Simple
import GI.Gio (ListStore, listStoreRemoveAll, listStoreSplice)
import Schema (threadUpdateMain)
import TypeWrappers (MissingBMS (MissingBMS), toMissingBMSWrapper)

showMissing :: Connection -> ListStore -> IO ()
showMissing conn listStore = do
  res <-
    query_
      conn
      "SELECT source_table, title, artist, level, url, url_diff, comment FROM bms_records WHERE NOT EXISTS (SELECT 1 FROM bms_files WHERE bms_records.md5 = bms_files.md5 OR bms_records.sha256 = bms_files.sha256)"
  close conn
  let missingRecords = (\(source_table, title, artist, level, url, url_diff, comment) -> MissingBMS source_table level artist title url url_diff comment) <$> res
  missingRecordsWrappers <- toMissingBMSWrapper missingRecords
  threadUpdateMain $ do
    listStoreRemoveAll listStore
    listStoreSplice listStore 0 0 missingRecordsWrappers
