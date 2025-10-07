{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import BMSFile (processBMS)
import Control.Monad (filterM, when)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Text as T
import Database.SQLite.Simple
import FetchTable (difficultyTables, getTables)
import PrettyPrint (showMissing)
import Schema
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, renameDirectory)
import System.FilePath (takeExtension, (</>))

readBMSRecords :: FilePath -> IO (Either String [BMSRecord])
readBMSRecords f = do
  jsonData <- B.readFile f
  return $ eitherDecode jsonData

processTable :: Connection -> FilePath -> IO ()
processTable conn f = do
  result <- readBMSRecords f
  let tableName = drop 7 $ takeWhile (/= '.') f
  case result of
    Left err -> putStrLn $ "Error parsing JSON file " ++ f ++ ": " ++ err
    Right records -> do
      mapM_ (insertRecord conn tableName) records
      putStrLn $ "Inserted " ++ show (length records) ++ " records from " ++ tableName

processTables :: [FilePath] -> IO ()
processTables filePaths = do
  conn <- open bmsDatabase
  execute_ conn "DROP TABLE IF EXISTS bms_records"
  createRecordTable conn
  mapM_ (processTable conn) filePaths
  close conn
  putStrLn $ "Processed " ++ show (length filePaths) ++ " tables."

processBMSFile :: Connection -> FilePath -> IO ()
processBMSFile conn f = do
  b <- processBMS f
  insertBMSFile conn f b

rebuildBMSFiles :: FilePath -> Connection -> IO ()
rebuildBMSFiles rootDir conn = do
  packs <- getSubdirectories rootDir
  mapM_ getFilesFromSubdirs packs
 where
  getSubdirectories :: FilePath -> IO [FilePath]
  getSubdirectories dir = do
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
    filterM doesDirectoryExist fullPaths

  getFilesFromSubdirs :: FilePath -> IO ()
  getFilesFromSubdirs dir = do
    subdirs <- getSubdirectories dir
    mapM_ insertFiles subdirs

  insertFiles :: FilePath -> IO ()
  insertFiles dir = do
    putStrLn dir
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
        bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
    mapM_ (processBMSFile conn) bmsFiles

addBMSFiles :: FilePath -> Connection -> IO ()
addBMSFiles rootDir conn = do
  packs <- getSubdirectories rootDir
  mapM_ getFilesFromSubdirs packs
 where
  getSubdirectories :: FilePath -> IO [FilePath]
  getSubdirectories dir = do
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
    filterM doesDirectoryExist fullPaths

  getFilesFromSubdirs :: FilePath -> IO ()
  getFilesFromSubdirs dir = do
    subdirs <- getSubdirectories dir
    mapM_ insertFiles subdirs

  insertFiles :: FilePath -> IO ()
  insertFiles dir = do
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
        bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
    mapM_ (processBMSFileIfExist conn) bmsFiles

processBMSFileIfExist :: Connection -> FilePath -> IO ()
processBMSFileIfExist conn f = do
  b <- fileNotExistInDb f
  when b $ do
    bms <- processBMS f
    insertBMSFile conn f bms
    putStrLn $ "Added " <> f
 where
  fileNotExistInDb :: FilePath -> IO Bool
  fileNotExistInDb file = do
    res <- query conn "SELECT 1 FROM bms_files WHERE file_path = ?" (Only file) :: IO [Only Int]
    return $ null res

renameBMSFolders :: FilePath -> IO ()
renameBMSFolders dir = do
  songs <- listDirectory dir
  let fullPaths = map (dir </>) songs
  f <- filterM doesDirectoryExist fullPaths
  mapM_ (renameBMSFolder dir) f

renameBMSFolder :: FilePath -> FilePath -> IO ()
renameBMSFolder parent dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
      bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
  b <- mapM processBMS bmsFiles
  let illegalCharacters =
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
      normalize x = T.unpack $ foldl' (\n (from, to) -> T.replace from to n) (T.strip $ commonPrefix x) illegalCharacters
      a = normalize $ fArtist <$> b
      t = normalize $ fTitle <$> b
      folderName = parent <> t <> " [" <> a <> "]"
  renameDirectory dir folderName

deleteBMSEntries :: IO ()
deleteBMSEntries = do
  conn <- open bmsDatabase
  entries <- query_ conn "SELECT file_path FROM bms_files"
  nonExistent <- filterM (fmap not . doesFileExist . T.unpack . fromOnly) entries
  mapM_ (execute conn "DELETE FROM bms_files WHERE file_path = ?") nonExistent
  close conn

main :: IO ()
main = do
  arg <- getLine
  case arg of
    "rebuild" -> do
      conn <- open bmsDatabase
      execute_ conn "DROP TABLE IF EXISTS bms_files"
      createFileTable conn
      rebuildBMSFiles bmsActualData conn
      close conn
      putStrLn "Rebuilt Database"
    "add" -> do
      conn <- open bmsDatabase
      addBMSFiles bmsActualData conn
      close conn
      putStrLn "Added New Songs"
    "fetch" -> do
      putStrLn "Fetching Tables"
      getTables
      putStrLn "Fetched Tables"
    "delete" -> do
      deleteBMSEntries
      putStrLn "Deleted Extra Entries"
    "load" -> do
      let jsonFiles = (<> ".json") . (tablesFolder <>) . fst <$> difficultyTables -- Replace with your JSON file names
      processTables jsonFiles
      putStrLn "Added All Tables"
    "rename" -> do
      renameBMSFolders (bmsActualData <> "Uncategorized/")
      putStrLn "Renamed Songs"
    (stripPrefix "i " -> Just songDirectory) -> do
      conn <- open bmsDatabase
      entries <- listDirectory songDirectory
      let fullPaths = map (songDirectory </>) entries
          bmsFiles = [f | f <- fullPaths, takeExtension f `elem` validExts]
      mapM_ (processBMSFile conn) bmsFiles
      putStrLn "Inserted One Song"
      close conn
    "show" -> do
      putStrLn "Writing Missing Files"
      showMissing
    _ -> return ()
  main
