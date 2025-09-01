{-# LANGUAGE OverloadedStrings #-}

module BMSFile where

import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16 (encode)
import Data.List
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as ICU
import Data.Text.Encoding (decodeUtf8)
import Schema

processBMS :: FilePath -> IO BMSFile
processBMS file = do
  bytestring <- B.readFile file
  result <- shiftJISToUTF8 bytestring
  let l = T.lines result
      digest = MD5.finalize $ MD5.update MD5.init bytestring
      g txt i = case find (T.isPrefixOf txt) l of
        Just x -> T.drop i x
        Nothing -> ""
  return
    BMSFile
      { fArtist = g "#ARTIST " 8
      , fTitle = g "#TITLE " 7
      , fMd5 = Just $ decodeUtf8 $ B16.encode digest
      , fSha256 = Nothing
      , filePath = T.pack file
      }

-- Convert Shift JIS to UTF-8 Text
shiftJISToUTF8 :: B.ByteString -> IO T.Text
shiftJISToUTF8 bs = do
  converter <- ICU.open "shift_jis" Nothing
  return $ ICU.toUnicode converter bs
