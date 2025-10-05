{-# LANGUAGE OverloadedStrings #-}

module BMSFile where

import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.ICU.Convert as ICU
import Schema (BMSFile (..))

-- getAllBMSFiles :: FilePath -> IO [BMSFile]

processBMS :: FilePath -> IO BMSFile
processBMS f = if "bmson" `isSuffixOf` f || "BMSON" `isSuffixOf` f then parseBMSON f else parseBMS f

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
      { fArtist = remove "#ARTIST " 8
      , fTitle = remove "#TITLE " 7
      , fMd5 = Just $ decodeUtf8 $ B16.encode digest
      , fSha256 = Nothing
      , filePath = T.pack file
      }

data BMSON = BMSON
  { bmsonTitle :: T.Text
  , bmsonArtist :: T.Text
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

parseBMSON :: FilePath -> IO BMSFile
parseBMSON file = do
  bytestring <- BL.readFile file
  bs <- B.readFile file
  let
    digest = SHA256.finalize $ SHA256.update SHA256.init bs
  case eitherDecode bytestring of
    Left err -> do
      putStrLn $ "Error parsing BMSON file " ++ file ++ ": " ++ err
      return BMSFile{fArtist = "", fTitle = "", fMd5 = Nothing, fSha256 = Nothing, filePath = ""}
    Right BMSONFile{info = BMSON{bmsonArtist = a, bmsonTitle = t}} ->
      return
        BMSFile
          { fArtist = a
          , fTitle = t
          , fMd5 = Nothing
          , fSha256 = Just $ decodeUtf8 $ B16.encode digest
          , filePath = T.pack file
          }

-- Convert Shift JIS to UTF-8 Text
shiftJISToUTF8 :: B.ByteString -> IO T.Text
shiftJISToUTF8 bs = do
  converter <- ICU.open "shift_jis" Nothing
  return $ ICU.toUnicode converter bs
