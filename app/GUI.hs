{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}

module GUI (startApp) where

import AppLogic
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forM_, forever, void, (>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Data.GI.Base
import Data.GI.Base.GObject (gobjectGetPrivateData, registerGType)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango
import Schema (BMSFile (..), Config (bmsFolder), LogMessage (..), threadUpdateMain)
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
  fileGType <- registerGType BMSFileWrapper

  missingListStore <- new Gio.ListStore [#itemType := missingGtype]
  fileListStore <- new Gio.ListStore [#itemType := fileGType]

  searchEntry <- new Gtk.SearchEntry [#searchDelay := 250]

  let buttons =
        [ ("Rebuild Database", rebuildDatabase fileListStore)
        , ("Add New Songs", addNewSongs fileListStore)
        , ("Clean Database", cleanDatabase fileListStore)
        , ("Fetch Tables", fetchTables)
        , ("Load Tables", loadTables)
        , ("Rename Uncategorized", renameUncategorized)
        , ("Show Missing Files", showMissingFiles missingListStore)
        ]

  buttonBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 5
      ]

  forM_ buttons $ \(buttonLabel, action) -> do
    new Gtk.Button [#label := buttonLabel, On #clicked (void $ forkIO $ action config logChan)] >>= #append buttonBox

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
      , #widthRequest := 200
      ]

  addMissingSimpleColumn missingColumnView "Artist" artist False
  addMissingSimpleColumn missingColumnView "Title" title True

  addButtonColumn missingColumnView "URL" url
  addButtonColumn missingColumnView "Diff" url_diff
  addToggleColumn missingColumnView

  addMissingSimpleColumn missingColumnView "Comment" (fromMaybe "" . comment) False
  addMissingSimpleColumn missingColumnView "Table" source_table False
  addMissingSimpleColumn missingColumnView "Level" level False

  let customFileFilterFunc =
        castTo BMSFileWrapper >=> \case
          Nothing -> pure False
          Just wrapper -> do
            priv <- gobjectGetPrivateData wrapper
            search <- get searchEntry #text
            let match t = T.toCaseFold search `T.isInfixOf` T.toCaseFold t
            pure $ match (fTitle priv) || match (fArtist priv)
  customFileFilter <- Gtk.customFilterNew (Just customFileFilterFunc)

  void $ on searchEntry #searchChanged $ #changed customFileFilter Gtk.FilterChangeDifferent

  #append buttonBox searchEntry

  fileFilterModel <-
    new
      Gtk.FilterListModel
      [ #model := fileListStore
      , #incremental := True
      , #filter := customFileFilter
      ]

  fileSelectionModel <- new Gtk.MultiSelection [#model := fileFilterModel]

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
      , #widthRequest := 200
      ]

  addFileSimpleColumn fileColumnView "Artist" fArtist False
  addFileSimpleColumn fileColumnView "Title" fTitle False

  let len = length $ bmsFolder config
  addFileSimpleColumn fileColumnView "Path" (T.drop len . filePath) True

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
logUpdater buffer textView chan = do
  e <- Gtk.textBufferGetEndIter buffer
  mark <- Gtk.textBufferCreateMark buffer Nothing e False
  forever $ do
    readChan chan >>= \case
      LogMessage text -> threadUpdateMain $ do
        endIter <- Gtk.textBufferGetEndIter buffer
        Gtk.textBufferInsert buffer endIter (text <> "\n") (-1)
        Gtk.textViewScrollToMark textView mark 0 False 0 0
      ClearLog -> threadUpdateMain $ do
        start <- Gtk.textBufferGetStartIter buffer
        end <- Gtk.textBufferGetEndIter buffer
        Gtk.textBufferDelete buffer start end

addMissingSimpleColumn :: Gtk.ColumnView -> T.Text -> (MissingBMS -> T.Text) -> Bool -> IO ()
addMissingSimpleColumn colView columnTitle getText expandable = do
  factory <- new Gtk.SignalListItemFactory []

  void $ on factory #setup $ \o -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o
      label <-
        new
          Gtk.Label
          [ #selectable := True
          , #halign := Gtk.AlignStart
          , #marginEnd := 5
          , #marginStart := 2
          , #marginBottom := 1
          , #marginTop := 1
          , #ellipsize := Pango.EllipsizeModeEnd
          ]
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
      Gtk.widgetSetTooltipText label (Just text)
    case res of
      Nothing -> putStrLn "bindListItem failed"
      Just () -> return ()

  column <-
    new
      Gtk.ColumnViewColumn
      [ #title := columnTitle
      , #factory := factory
      , #expand := expandable
      , #resizable := True
      , #fixedWidth := 270
      ]

  Gtk.columnViewAppendColumn colView column

addFileSimpleColumn :: Gtk.ColumnView -> T.Text -> (BMSFile -> T.Text) -> Bool -> IO ()
addFileSimpleColumn colView columnTitle getText expandable = do
  factory <- new Gtk.SignalListItemFactory []

  void $ on factory #setup $ \o -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o
      label <-
        new
          Gtk.Label
          [ #selectable := True
          , #halign := Gtk.AlignStart
          , #marginEnd := 5
          , #marginStart := 2
          , #marginBottom := 1
          , #marginTop := 1
          , #ellipsize := Pango.EllipsizeModeEnd
          ]
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
      set label [#label := text, #tooltipText := text]
    case res of
      Nothing -> putStrLn "bindListItem failed"
      Just () -> return ()

  column <-
    new
      Gtk.ColumnViewColumn
      [ #title := columnTitle
      , #factory := factory
      , #expand := expandable
      , #resizable := True
      , #fixedWidth := 300
      ]

  Gtk.columnViewAppendColumn colView column

addButtonColumn :: Gtk.ColumnView -> T.Text -> (MissingBMS -> Maybe T.Text) -> IO ()
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
      uri <- hoistMaybe $ getURL bmsRecord
      set btn [#uri := uri, #label := "⬇", #tooltipText := uri]
    case res of
      Nothing -> putStrLn "bindListItem failed"
      Just () -> return ()

  column <-
    new
      Gtk.ColumnViewColumn
      [ #title := columnTitle
      , #factory := factory
      , #expand := False
      , #resizable := True
      , #fixedWidth := 70
      ]

  Gtk.columnViewAppendColumn colView column

addToggleColumn :: Gtk.ColumnView -> IO ()
addToggleColumn colView = do
  factory <- new Gtk.SignalListItemFactory []

  void $ on factory #setup $ \o -> do
    res <- runMaybeT $ do
      listItem <- MaybeT $ castTo Gtk.ListItem o
      btn <- new Gtk.CheckButton [#label := "Done"]
      #setChild listItem (Just btn)
    case res of
      Nothing -> putStrLn "initListItem failed"
      Just () -> return ()

  void $ on factory #bind $ \_ -> pure ()

  column <-
    new
      Gtk.ColumnViewColumn
      [ #title := "Check"
      , #factory := factory
      , #expand := False
      , #resizable := True
      , #fixedWidth := 120
      ]

  Gtk.columnViewAppendColumn colView column
