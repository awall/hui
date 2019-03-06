{-# LANGUAGE Arrows #-}
module DemoText where

import Hui.Hairo as Hairo
import Hui.Auto
import Hui.Event
import Hui.Color
import Hui.Focus
import Hui.Geometry
import Hui.Gtk

import Control.Arrow
import Control.Monad (liftM, join)
import Data.Maybe
import qualified Data.Text as T

main :: IO ()
main = do
  mainLoop drawAll

drawAll :: Auto Event Draw
drawAll = proc e -> do
  Size w h <- windowBounds -< e

  let b1 = Rect (XY 0.0  10.0) (XY 150.0  30.0)
  let b2 = Rect (XY 0.0  60.0) (XY 150.0  80.0)
  let b3 = Rect (XY 0.0 110.0) (XY 150.0 130.0)
  f <- focus -< (e, [b1,b2,b3])

  w1 <- textBoxWidget -< (b1, e, f b1)
  w2 <- textBoxWidget -< (b2, e, f b2)
  w3 <- textBoxWidget -< (b3, e, f b3)

  returnA -< do Hairo.setSourceRGBA white
                Hairo.rectangle (Rect (XY 0.0 0.0) (XY w h))
                Hairo.fill
                w1
                w2
                w3

textBoxWidget :: Auto (Rect, Event, Bool) Draw
textBoxWidget = proc (Rect (XY l t) (XY r b), e, hasFocus) -> do
  let k = if hasFocus then keyDowns e else Nothing
  te <- text -< k
  returnA -< do Hairo.save
                if hasFocus
                   then Hairo.setSourceRGBA red
                   else Hairo.setSourceRGBA gray
                Hairo.rectangle (Rect (XY l t) (XY r b))
                Hairo.clip
                Hairo.rectangle (Rect (XY l t) (XY r b))
                Hairo.fill
                Hairo.setSourceRGBA black
                Hairo.moveTo (XY l (t + 18.0))
                Hairo.showText te
                Hairo.restore

bounds :: Auto Event Size
bounds = proc e -> do
  remember undefined -< paints e

mousePos :: Auto Event (Maybe XY)
mousePos = proc e -> do
  remember Nothing -< case e of EMouseDown xy -> Just (Just xy)
                                EMouseUp   xy -> Just (Just xy)
                                EMouseMove xy -> Just (Just xy)
                                _             -> Nothing

text :: Auto (Maybe Key) T.Text
text = proc k -> do
  accumA append T.empty -< k
  where append t (Just KBack)     = if T.null t then T.empty else T.init t
        append t (Just (KChar c)) = T.snoc t c
        append t _                = t
