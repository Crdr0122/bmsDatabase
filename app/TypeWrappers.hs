{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TypeWrappers (
  MissingBMSWrapper (..),
  MissingBMS (..),
  BMSFileWrapper (..),
  newMissingBMSWrapper,
  toMissingBMSWrapper,
  newBMSFileWrapper,
  toBMSFileWrapper,
) where

import Control.Monad.IO.Class
import Data.GI.Base (
  GObject,
  ManagedPtr,
  TypedObject (..),
  glibType,
 )
import Data.GI.Base.GObject (DerivedGObject (..), gobjectSetPrivateData, registerGType)
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.Text (Text)
import GI.GObject (Object, new, toObject)
import Schema (BMSFile (..))

data MissingBMS = MissingBMS
  { source_table :: Text
  , level :: Text
  , artist :: Text
  , title :: Text
  , url :: Maybe Text
  , url_diff :: Maybe Text
  , comment :: Maybe Text
  }
  deriving (Show)

newtype MissingBMSWrapper = MissingBMSWrapper (ManagedPtr MissingBMSWrapper)

instance TypedObject MissingBMSWrapper where
  glibType = registerGType MissingBMSWrapper

instance GObject MissingBMSWrapper

instance DerivedGObject MissingBMSWrapper where
  type GObjectParentType MissingBMSWrapper = Object

  type GObjectPrivateData MissingBMSWrapper = MissingBMS

  objectTypeName = "MyTypeObject"

  objectClassInit _ = pure ()

  objectInstanceInit _ _ =
    pure $
      MissingBMS
        { source_table = ""
        , level = ""
        , artist = ""
        , title = ""
        , url = Nothing
        , url_diff = Nothing
        , comment = Nothing
        }

  objectInterfaces = []

instance HasParentTypes MissingBMSWrapper

type instance ParentTypes MissingBMSWrapper = '[Object]

newMissingBMSWrapper :: (MonadIO m) => MissingBMS -> m Object
newMissingBMSWrapper record = liftIO $ do
  wrapper <- new MissingBMSWrapper []
  gobjectSetPrivateData wrapper record
  o <- toObject wrapper
  pure o

toMissingBMSWrapper :: (MonadIO m) => [MissingBMS] -> m [Object]
toMissingBMSWrapper = mapM newMissingBMSWrapper

-- BMSFile Wrapper

newtype BMSFileWrapper = BMSFileWrapper (ManagedPtr BMSFileWrapper)

instance TypedObject BMSFileWrapper where
  glibType = registerGType BMSFileWrapper

instance GObject BMSFileWrapper

instance DerivedGObject BMSFileWrapper where
  type GObjectParentType BMSFileWrapper = Object

  type GObjectPrivateData BMSFileWrapper = BMSFile

  objectTypeName = "MyTypeObject"

  objectClassInit _ = pure ()

  objectInstanceInit _ _ =
    pure $
      BMSFile
        { fArtist = ""
        , fTitle = ""
        , fMd5 = Nothing
        , fSha256 = Nothing
        , filePath = ""
        }

  objectInterfaces = []

instance HasParentTypes BMSFileWrapper

type instance ParentTypes BMSFileWrapper = '[Object]

newBMSFileWrapper :: (MonadIO m) => BMSFile -> m Object
newBMSFileWrapper file = liftIO $ do
  wrapper <- new BMSFileWrapper []
  gobjectSetPrivateData wrapper file
  o <- toObject wrapper
  pure o

toBMSFileWrapper :: (MonadIO m) => [BMSFile] -> m [Object]
toBMSFileWrapper = mapM newBMSFileWrapper
