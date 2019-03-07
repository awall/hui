{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Hui.Win32 where

import Hui.Auto
import Hui.Event
import Hui.Geometry
import Hui.Hairo(Draw)

import Control.Monad.Reader(ask, liftIO)
import Control.Monad.Identity(runIdentity)
import Control.Exception (SomeException, catch)
import Data.Char(chr)
import Data.IORef
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types
import Foreign.C
import qualified Graphics.Win32 as Win32
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal(mkSurface,Cairo,unCairo)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.Win32.DLL (getModuleHandle)

foreign import ccall "cairo-win32.h cairo_win32_surface_create"
  cairo_win32_surface_create :: Win32.HDC -> IO (Ptr Cairo.Surface)

createWin32Surface :: Win32.HDC -> IO Cairo.Surface
createWin32Surface hdc = cairo_win32_surface_create hdc >>= mkSurface



mainLoop :: Auto Event Draw -> IO ()
mainLoop draw = Win32.allocaPAINTSTRUCT $ \lpps -> do
  ref <- newIORef draw
  hwnd <- newWindow 200 200 (wndProc lpps ref)
  messagePump hwnd

newWindow :: Int -> Int -> Win32.WindowClosure -> IO Win32.HWND
newWindow width height wndProc = do
  let winClass = Win32.mkClassName "Hello"
  icon <- Win32.loadIcon Nothing Win32.iDI_APPLICATION
  cursor <- Win32.loadCursor Nothing Win32.iDC_ARROW
  bgBrush <- Win32.createSolidBrush (Win32.rgb 0 0 255)
  mainInstance <- getModuleHandle Nothing
  Win32.registerClass (Win32.cS_VREDRAW + Win32.cS_HREDRAW
    , mainInstance
    , Just icon
    , Just cursor
    , Just bgBrush
    , Nothing
    , winClass)
  w <- Win32.createWindow
    winClass
    "Hello, World example"
    Win32.wS_OVERLAPPEDWINDOW
    Nothing Nothing -- leave it to the shell to decide the position
    (Just width)
    (Just height)
    Nothing -- no parent, i.e, root window is the parent.
    Nothing -- no menu handle
    mainInstance
    wndProc
  Win32.showWindow w Win32.sW_SHOWNORMAL
  Win32.updateWindow w
  return w

messagePump :: Win32.HWND -> IO ()
messagePump hwnd = Win32.allocaMessage $ \msg ->
  let pump = do Win32.getMessage msg (Just hwnd)
                  `catch` \(_::SomeException) -> exitWith ExitSuccess
                Win32.translateMessage msg
                Win32.dispatchMessage msg
                pump
  in pump


wndProc :: Win32.LPPAINTSTRUCT
        -> IORef (Auto Event Draw)
        -> Win32.HWND
        -> Win32.WindowMessage
        -> Win32.WPARAM
        -> Win32.LPARAM
        -> IO Win32.LRESULT
wndProc lpps ref hwnd msg wParam lParam
  | msg == Win32.wM_DESTROY     = do Win32.sendMessage hwnd Win32.wM_QUIT 1 0
                                     return 0

  | msg == Win32.wM_PAINT       = do (_,_,w,h) <- Win32.getClientRect hwnd
                                     draw <- process ref $ EPaint (XY (fromIntegral w) (fromIntegral h))
                                     paintFrame draw lpps hwnd (w,h)
                                     return 0

  | msg == Win32.wM_LBUTTONDOWN = do let dword = fromIntegral lParam :: Win32.DWORD
                                         x = fromIntegral $ Win32.lOWORD dword :: Double
                                         y = fromIntegral $ Win32.hIWORD dword :: Double
                                     process ref $ EMouseDown (XY x y)
                                     Win32.invalidateRect (Just hwnd) Nothing False
                                     return 0

  | msg == Win32.wM_LBUTTONUP   = do let dword = fromIntegral lParam :: Win32.DWORD
                                         x = fromIntegral $ Win32.lOWORD dword :: Double
                                         y = fromIntegral $ Win32.hIWORD dword :: Double
                                     process ref $ EMouseUp (XY x y)
                                     Win32.invalidateRect (Just hwnd) Nothing False
                                     return 0

  | msg == Win32.wM_MOUSEMOVE   = do let dword = fromIntegral lParam :: Win32.DWORD
                                         x = fromIntegral $ Win32.lOWORD dword :: Double
                                         y = fromIntegral $ Win32.hIWORD dword :: Double
                                     process ref $ EMouseMove (XY x y)
                                     Win32.invalidateRect (Just hwnd) Nothing False
                                     return 0

  | msg == Win32.wM_CHAR        = do let vkey   = fromIntegral wParam
                                     let key = if | vkey == Win32.vK_BACK -> KBack
                                                  | vkey == Win32.vK_TAB  -> KTab
                                                  | otherwise             -> KChar $ chr $ fromIntegral vkey
                                     process ref $ EKeyDown key
                                     Win32.invalidateRect (Just hwnd) Nothing False
                                     return 0

  | msg == Win32.wM_ERASEBKGND  = do return 1

  | otherwise                   = do Win32.defWindowProc (Just hwnd) msg wParam lParam

process :: IORef (Auto i o) -> i -> IO o
process ref e = do auto <- readIORef ref
                   let (cur, next) = runIdentity $ runAutoM auto e
                   writeIORef ref next
                   return cur

paintFrame :: Draw -> Win32.LPPAINTSTRUCT -> Win32.HWND -> (Win32.LONG, Win32.LONG) -> IO ()
paintFrame draw lpps hwnd (w, h) = do
  front_hdc <- Win32.beginPaint hwnd lpps
  back_hdc <- Win32.createCompatibleDC (Just front_hdc)
  bmp <- Win32.createCompatibleBitmap front_hdc w h
  old_bmp <- Win32.selectBitmap back_hdc bmp
  surface <- createWin32Surface back_hdc
  Cairo.renderWith surface $ draw
  Win32.bitBlt front_hdc 0 0 w h back_hdc 0 0 Win32.sRCCOPY
  Win32.selectBitmap back_hdc old_bmp
  Win32.deleteBitmap bmp
  Win32.deleteDC back_hdc
  Win32.endPaint hwnd lpps
