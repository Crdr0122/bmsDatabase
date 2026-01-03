{-# LANGUAGE OverloadedStrings #-}

module BMSFile where

import Control.Monad (filterM, when)
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (find, isSuffixOf)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.ICU.Convert as ICU
import Database.SQLite.Simple
import Schema (BMSFile (..), insertBMSFile, normalizeTitle, validExts)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, renameDirectory)
import System.FilePath (takeExtension, (</>))

data BMSON = BMSON
  { bmsonTitle :: T.Text,
    bmsonArtist :: T.Text
  }
  deriving (Show)

newtype BMSONFile = BMSONFile {info :: BMSON}

instance FromJSON BMSON where
  parseJSON = withObject "BMSON" $ \v ->
    BMSON
      <$> v .: "artist"
      <*> v .: "title"

instance FromJSON BMSONFile where
  parseJSON = withObject "BMSONFile" $ \v ->
    BMSONFile
      <$> v .: "info"



processBMS :: FilePath -> IO BMSFile
processBMS f = if "bmson" `isSuffixOf` f || "BMSON" `isSuffixOf` f then parseBMSON f else parseBMS f

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
  let a = normalizeTitle $ fArtist <$> b
      t = normalizeTitle $ fTitle <$> b
      folderName = parent <> t <> " [" <> a <> "]"
  renameDirectory dir folderName

deleteBMSEntries :: Connection -> IO ()
deleteBMSEntries conn = do
  entries <- query_ conn "SELECT file_path FROM bms_files"
  nonExistent <- filterM (fmap not . doesFileExist . T.unpack . fromOnly) entries
  mapM_ (execute conn "DELETE FROM bms_files WHERE file_path = ?") nonExistent
  close conn

parseBMS :: FilePath -> IO BMSFile
parseBMS file = do
  bytestring <- B.readFile file
  result <- shiftJISToUTF8 bytestring
  let l = T.lines result
      digest = MD5.finalize $ MD5.update MD5.init bytestring
      remove txt i = case find (T.isPrefixOf txt) l of
        Just x -> T.stripEnd $ T.drop i x
        Nothing -> ""
  return
    BMSFile
      { fArtist = remove "#ARTIST " 8,
        fTitle = remove "#TITLE " 7,
        fMd5 = Just $ decodeUtf8 $ B16.encode digest,
        fSha256 = Nothing,
        filePath = T.pack file
      }

parseBMSON :: FilePath -> IO BMSFile
parseBMSON file = do
  bytestring <- BL.readFile file
  bs <- B.readFile file
  let digest = SHA256.finalize $ SHA256.update SHA256.init bs
  case eitherDecode bytestring of
    Left err -> do
      putStrLn $ "Error parsing BMSON file " ++ file ++ ": " ++ err
      return BMSFile {fArtist = "", fTitle = "", fMd5 = Nothing, fSha256 = Nothing, filePath = ""}
    Right BMSONFile {info = BMSON {bmsonArtist = a, bmsonTitle = t}} ->
      return
        BMSFile
          { fArtist = a,
            fTitle = t,
            fMd5 = Nothing,
            fSha256 = Just $ decodeUtf8 $ B16.encode digest,
            filePath = T.pack file
          }

shiftJISToUTF8 :: B.ByteString -> IO T.Text
shiftJISToUTF8 bs = do
  converter <- ICU.open "shift_jis" Nothing
  return $ ICU.toUnicode converter bs
