module PrettyPrint (showMissing) where

import Control.Monad (void)
import Database.SQLite.Simple
import GI.GLib qualified as GLib
import GI.Gio (ListStore, listStoreRemoveAll, listStoreSplice)
import TypeWrappers (MissingBMS (MissingBMS), toMissingBMSWrapper)

showMissing :: Connection -> ListStore -> IO ()
showMissing conn listStore = do
  res <-
    query_
      conn
      "SELECT source_table, title, artist, level, url, url_diff, comment FROM bms_records WHERE NOT EXISTS (SELECT 1 FROM bms_files WHERE bms_records.md5 = bms_files.md5 OR bms_records.sha256 = bms_files.sha256)"
  close conn
  let missingRecords =
        ( \(source_table, title, artist, level, url, url_diff, comment) ->
            MissingBMS source_table level artist title url url_diff comment
        )
          <$> res
  missingRecordsWrappers <- toMissingBMSWrapper missingRecords
  void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
    listStoreRemoveAll listStore
    listStoreSplice listStore 0 0 missingRecordsWrappers
    return False
