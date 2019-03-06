{-# LANGUAGE Arrows #-}
module Hui.Event where

import Hui.Auto
import Hui.Geometry

import Control.Arrow
import Control.Category((.))
import Prelude hiding ((.))

data Key = KBack | KTab | KChar Char

data Event =
  EPaint     XY  |
  EKeyDown   Key |
  EMouseDown XY  |
  EMouseUp   XY  |
  EMouseMove XY

windowBounds :: Auto Event Rect
windowBounds = fmap (`at` XY 0 0) $ (remember undefined . arr paints)

clicks :: Auto Event XY
clicks = proc e -> do
  remember (XY 0 0) -< case e of EMouseDown xy -> Just xy
                                 _             -> Nothing

paints :: Event -> Maybe XY
paints (EPaint b) = Just b
paints         _  = Nothing

mouseDowns :: Event -> Maybe XY
mouseDowns (EMouseDown xy) = Just xy
mouseDowns               _ = Nothing

mouseMoves :: Event -> Maybe XY
mouseMoves (EMouseMove xy) = Just xy
mouseMoves               _ = Nothing

mouseUps :: Event -> Maybe XY
mouseUps (EMouseUp xy) = Just xy
mouseUps             _ = Nothing

keyDowns :: Event -> Maybe Key
keyDowns (EKeyDown k) = Just k
keyDowns            _ = Nothing
