{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GUI (startApp) where

import AppLogic
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forM_, forever, unless, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.GI.Base
import Data.GI.Base.GObject (gobjectGetPrivateData, registerGType)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Schema (BMSFile (..), Config (bmsFolder), LogMessage (..))
import TypeWrappers (BMSFileWrapper (..), MissingBMS (..), MissingBMSWrapper (..))

startApp :: Config -> IO ()
startApp config = do
  logChan <- newChan
  app <-
    new
      Gtk.Application
      [ #applicationId := "com.bms.manager"
      , On #activate (activate ?self config logChan)
      ]
  void $ #run app Nothing

activate :: Gtk.Application -> Config -> Chan LogMessage -> IO ()
activate app config logChan = do
  topHalfBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 10
      ]

  missingGtype <- registerGType MissingBMSWrapper
  missingListStore <-
    new
      Gio.ListStore
      [#itemType := missingGtype]

  let buttons =
        [ ("Rebuild Database", rebuildDatabase)
        , ("Add New Songs", addNewSongs)
        , ("Fetch Tables", fetchTables)
        , ("Clean Database", cleanDatabase)
        , ("Load Tables", loadTables)
        , ("Rename Uncategorized", renameUncategorized)
        , ("Show Missing Files", showMissingFiles missingListStore)
        ]

  buttonBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 3
      ]

  forM_ buttons $ \(buttonLabel, action) -> do
    button <-
      new
        Gtk.Button
        [ #label := buttonLabel
        , On #clicked (void $ forkIO $ action config logChan)
        ]
    #append buttonBox button

  consoleBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 10
      ]

  consoleLabel <-
    new
      Gtk.Label
      [ #label := "Console Output:"
      , #halign := Gtk.AlignStart
      ]

  (logTextView, logBuffer) <- createLogArea

  consoleScroll <-
    new
      Gtk.ScrolledWindow
      [ #hexpand := True
      , #minContentHeight := 250
      , #marginTop := 12
      , #marginBottom := 12
      , #marginStart := 12
      , #marginEnd := 12
      , #child := logTextView
      ]

  consoleFrame <-
    new
      Gtk.Frame
      [#child := consoleScroll]

  clearButton <-
    new
      Gtk.Button
      [ #label := "Clear Console"
      , On #clicked (writeChan logChan ClearLog)
      ]

  missingSelectionModel <-
    new
      Gtk.SingleSelection
      [ #model := missingListStore
      ]
  missingColumnView <-
    new
      Gtk.ColumnView
      [ #model := missingSelectionModel
      , #showColumnSeparators := True
      , #showRowSeparators := True
      , #reorderable := False
      ]

  missingScrollWindow <-
    new
      Gtk.ScrolledWindow
      [ #child := missingColumnView
      , #hexpand := True
      , #widthRequest := 300
      ]

  addMissingSimpleColumn missingColumnView "Table" source_table
  addMissingSimpleColumn missingColumnView "Level" level
  addMissingSimpleColumn missingColumnView "Artist" artist
  addMissingSimpleColumn missingColumnView "Title" title

  addButtonColumn missingColumnView "URL" (fromMaybe "" . url)
  addButtonColumn missingColumnView "Diff" (fromMaybe "" . url_diff)

  addMissingSimpleColumn missingColumnView "Comment" (fromMaybe "" . comment)

  fileGType <- registerGType MissingBMSWrapper
  fileListStore <-
    new
      Gio.ListStore
      [#itemType := fileGType]

  fileFilterModel <-
    new
      Gtk.FilterListModel
      [ #model := fileListStore
      ]

  fileSelectionModel <-
    new
      Gtk.MultiSelection
      [ #model := fileFilterModel
      ]
  fileColumnView <-
    new
      Gtk.ColumnView
      [ #model := fileSelectionModel
      , #showColumnSeparators := True
      , #showRowSeparators := True
      ]
  fileScrollWindow <-
    new
      Gtk.ScrolledWindow
      [ #child := fileColumnView
      , #hexpand := True
      , #widthRequest := 300
      ]
  let len = length $ bmsFolder config
  addFileSimpleColumn fileColumnView "Artist" fArtist
  addFileSimpleColumn fileColumnView "Title" fTitle
  addFileSimpleColumn fileColumnView "Path" (T.drop len . filePath)

  bottomBox <-
    new
      Gtk.Paned
      [ #orientation := Gtk.OrientationHorizontal
      , #startChild := fileScrollWindow
      , #endChild := missingScrollWindow
      , #shrinkEndChild := False
      , #shrinkStartChild := False
      , #vexpand := True
      ]

  mainBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #marginTop := 12
      , #marginBottom := 12
      , #marginStart := 12
      , #marginEnd := 12
      ]

  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app
      , #title := "BMS Manager"
      , #child := mainBox
      ]

  #append consoleBox consoleLabel
  #append consoleBox consoleFrame
  #append consoleBox clearButton
  #append topHalfBox buttonBox
  #append topHalfBox consoleBox

  #append mainBox topHalfBox
  #append mainBox bottomBox

  void $ forkIO $ logUpdater logBuffer logTextView logChan
  void $ forkIO $ showAllFiles fileListStore config

  #show window
  return ()

createLogArea :: IO (Gtk.TextView, Gtk.TextBuffer)
createLogArea = do
  textView <-
    new
      Gtk.TextView
      [ #editable := False
      , #cursorVisible := False
      , #monospace := True
      , #wrapMode := Gtk.WrapModeWordChar
      ]

  buffer <- Gtk.textViewGetBuffer textView
  return (textView, buffer)

logUpdater :: Gtk.TextBuffer -> Gtk.TextView -> Chan LogMessage -> IO ()
logUpdater buffer textView chan = forever $ do
  msg <- readChan chan
  case msg of
    LogMessage text -> do
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
        end <- Gtk.textBufferGetEndIter buffer
        let logLine = text <> "\n"
        Gtk.textBufferInsert buffer end logLine (-1)
        mark <- Gtk.textBufferCreateMark buffer Nothing end True
        Gtk.textViewScrollToMark textView mark 0 False 0 0
        return False
    ClearLog -> do
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
        start <- Gtk.textBufferGetStartIter buffer
        end <- Gtk.textBufferGetEndIter buffer
        Gtk.textBufferDelete buffer start end
        return False

addMissingSimpleColumn :: (MonadIO m) => Gtk.ColumnView -> T.Text -> (MissingBMS -> T.Text) -> m ()
addMissingSimpleColumn colView columnTitle getText = do
  factory <- new Gtk.SignalListItemFactory []

  void $ on factory #setup $ \o -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o
      label <- new Gtk.Label [#selectable := True, #halign := Gtk.AlignStart]
      #setChild listItem (Just label)
    case res of
      Nothing -> putStrLn "initListItem failed"
      Just () -> return ()

  void $ on factory #bind $ \o1 -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o1
      o2 <- MaybeT $ Gtk.listItemGetItem listItem
      missingBMSWrapper <- MaybeT $ castTo MissingBMSWrapper o2
      widget <- MaybeT $ #getChild listItem
      label <- MaybeT $ castTo Gtk.Label widget
      bmsRecord <- liftIO $ gobjectGetPrivateData missingBMSWrapper
      let text = getText bmsRecord
      set label [#label := text]
      Gtk.widgetSetTooltipText label (Just (text))
    case res of
      Nothing -> putStrLn "bindListItem failed"
      Just () -> return ()

  column <-
    new
      Gtk.ColumnViewColumn
      [ #title := columnTitle
      , #factory := factory
      , #expand := True
      ]

  Gtk.columnViewAppendColumn colView column

addFileSimpleColumn :: (MonadIO m) => Gtk.ColumnView -> T.Text -> (BMSFile -> T.Text) -> m ()
addFileSimpleColumn colView columnTitle getText = do
  factory <- new Gtk.SignalListItemFactory []

  void $ on factory #setup $ \o -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o
      label <- new Gtk.Label [#selectable := True, #halign := Gtk.AlignStart]
      #setChild listItem (Just label)
    case res of
      Nothing -> putStrLn "initListItem failed"
      Just () -> return ()

  void $ on factory #bind $ \o1 -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o1
      o2 <- MaybeT $ Gtk.listItemGetItem listItem
      fileBMSWrapper <- MaybeT $ castTo BMSFileWrapper o2
      widget <- MaybeT $ #getChild listItem
      label <- MaybeT $ castTo Gtk.Label widget
      bmsFile <- liftIO $ gobjectGetPrivateData fileBMSWrapper
      let text = getText bmsFile
      set label [#label := text]
      Gtk.widgetSetTooltipText label (Just (text))
    case res of
      Nothing -> putStrLn "bindListItem failed"
      Just () -> return ()

  column <-
    new
      Gtk.ColumnViewColumn
      [ #title := columnTitle
      , #factory := factory
      , #expand := True
      -- , #fixedWidth := 100
      ]

  Gtk.columnViewAppendColumn colView column

addButtonColumn :: (MonadIO m) => Gtk.ColumnView -> T.Text -> (MissingBMS -> T.Text) -> m ()
addButtonColumn colView columnTitle getURL = do
  factory <- new Gtk.SignalListItemFactory []

  void $ on factory #setup $ \o -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o
      btn <- new Gtk.LinkButton []
      #setChild listItem (Just btn)
    case res of
      Nothing -> putStrLn "initListItem failed"
      Just () -> return ()

  void $ on factory #bind $ \o1 -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o1
      o2 <- MaybeT $ Gtk.listItemGetItem listItem
      missingBMSWrapper <- MaybeT $ castTo MissingBMSWrapper o2
      widget <- MaybeT $ #getChild listItem
      btn <- MaybeT $ castTo Gtk.LinkButton widget
      bmsRecord <- liftIO $ gobjectGetPrivateData missingBMSWrapper
      let uri = getURL bmsRecord
      unless (T.null uri) $ do
        set btn [#uri := uri, #label := "⬇"]
        Gtk.widgetSetTooltipText btn (Just (uri))
    case res of
      Nothing -> putStrLn "bindListItem failed"
      Just () -> return ()

  column <-
    new
      Gtk.ColumnViewColumn
      [ #title := columnTitle
      , #factory := factory
      , #expand := False
      ]

  Gtk.columnViewAppendColumn colView column
