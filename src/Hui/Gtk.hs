module Hui.Gtk (
  mainLoop
) where

import Hui.Auto
import Hui.Event
import Hui.Geometry
import Hui.Hairo(Draw)

import Control.Monad.Trans (liftIO)
import Data.IORef


import qualified Graphics.UI.Gtk as G
-- import qualified Graphics.UI.Gtk.Windows as G (windowHasResizeGrip)
import qualified System.Glib.UTFString as G (glibToString)



mainLoop :: Auto Event Draw -> IO ()
mainLoop auto = do
  G.initGUI
  window <- G.windowNew
  configureNetwork window auto
  G.on window G.deleteEvent $ do
    liftIO G.mainQuit
    return False
  G.widgetSetAppPaintable window True
  G.set window [ G.windowResizable G.:= False ]
  G.windowSetDefaultSize window 500 500
  G.widgetShowAll window
  G.mainGUI

configureNetwork :: G.Window -> Auto Event Draw -> IO ()
configureNetwork canvas auto = do
  stateRef <- newIORef (Nothing, auto)
  G.on canvas G.draw $ do
     (prev, _) <- liftIO $ readIORef stateRef
     case prev of
       Just d  -> d
       Nothing -> return ()

  let enqueue e = liftIO $ advance stateRef e
  let listen e m = G.on canvas e $ do
        m
        liftIO $ G.widgetQueueDraw canvas
        return False

  listen G.configureEvent $ do
    (w, h) <- G.eventSize
    enqueue $ EPaint (XY (fromIntegral w) (fromIntegral h))

  listen G.keyPressEvent $ do
    key <- G.eventKeyVal
    case G.keyToChar key of
      Just k  -> enqueue $ EKeyDown (KChar k)
      Nothing -> case G.glibToString (G.keyName key) of
        "BackSpace" -> enqueue $ EKeyDown KBack
        "Tab"       -> enqueue $ EKeyDown KTab
        _           -> return ()

  listen G.buttonPressEvent $ do
    (x, y) <- G.eventCoordinates
    enqueue $ EMouseDown (XY x y)

  listen G.buttonReleaseEvent $ do
    (x, y) <- G.eventCoordinates
    enqueue $ EMouseUp (XY x y)

  G.widgetAddEvents canvas [G.PointerMotionMask, G.PointerMotionHintMask]
  listen G.motionNotifyEvent $ do
    (x, y) <- G.eventCoordinates
    enqueue $ EMouseMove (XY x y)

  return ()

advance :: IORef (Maybe o, Auto i o) -> i -> IO ()
advance stateRef i = do
  (_, auto) <- readIORef stateRef
  let (curr, next) = runAuto auto i
  writeIORef stateRef (Just curr, next)
