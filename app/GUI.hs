{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI (startApp, LogMessage (..)) where

import AppLogic
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forM_, forever, void)
import Data.GI.Base
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import Schema

startApp :: Config -> IO ()
startApp config = do
  logChan <- newChan
  app <-
    new
      Gtk.Application
      [ #applicationId := "com.bms.manager",
        On #activate (activate ?self config logChan)
      ]
  void $ #run app Nothing

activate :: Gtk.Application -> Config -> Chan LogMessage -> IO ()
activate app config logChan = do
  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := "BMS Manager"
      ]
  mainBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #spacing := 10,
        #marginTop := 12,
        #marginBottom := 12,
        #marginStart := 12,
        #marginEnd := 12
      ]

  let buttons =
        [ ("Rebuild Database", rebuildDatabase),
          ("Add New Songs", addNewSongs),
          ("Fetch Tables", fetchTables),
          ("Clean Database", cleanDatabase),
          ("Load Tables", loadTables),
          ("Rename Uncategorized", renameUncategorized),
          ("Show Missing Files", showMissingFiles)
        ]

  buttonBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #spacing := 5
      ]

  forM_ buttons $ \(buttonLabel, action) -> do
    button <-
      new
        Gtk.Button
        [ #label := buttonLabel,
          On #clicked (void $ forkIO $ action config logChan)
        ]
    #append buttonBox button

  consoleLabel <-
    new
      Gtk.Label
      [ #label := "Console Output:",
        #halign := Gtk.AlignStart
      ]

  scrolledWindow <-
    new
      Gtk.ScrolledWindow
      [ #vexpand := True,
        #hexpand := True,
        #minContentHeight := 200
      ]

  (logTextView, logBuffer) <- createLogArea
  #setChild scrolledWindow (Just logTextView)

  clearButton <-
    new
      Gtk.Button
      [ #label := "Clear Console",
        On #clicked (writeChan logChan ClearLog)
      ]

  #append mainBox buttonBox
  #append mainBox consoleLabel
  #append mainBox scrolledWindow
  #append mainBox clearButton

  #setChild window (Just mainBox)

  void $ forkIO $ logUpdater logBuffer logTextView logChan

  #show window
  return ()

createLogArea :: IO (Gtk.TextView, Gtk.TextBuffer)
createLogArea = do
  textView <-
    new
      Gtk.TextView
      [ #editable := False,
        #cursorVisible := False,
        #monospace := True,
        #wrapMode := Gtk.WrapModeWordChar
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
