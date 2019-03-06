{-# LANGUAGE Arrows #-}

module Hui.Focus (focus) where

import Hui.Auto
import Hui.Event
import Hui.Geometry

import Control.Arrow
import Control.Monad

data Focus = NoFocus | FocusAt Int

findFocus :: [Rect] -> XY -> Focus
findFocus rs xy =
  let fs = filter (\(_, r) -> inside r xy) (zip [0..] rs)
   in case fs of [] -> NoFocus
                 ((i, _):_) -> FocusAt i

tabShifts :: Int -> Event -> Int
tabShifts x (EMouseDown _)  = 0
tabShifts x (EKeyDown KTab) = x + 1
tabShifts x _               = x

focus :: Auto (Event, [Rect]) (Rect -> Bool)
focus = proc (e, rs) -> do
  ts <- accumA tabShifts 0 -< e
  f  <- remember NoFocus -< liftM (findFocus rs) $ mouseDowns e
  returnA -< case (ts, f) of (0, NoFocus)   -> const False
                             (t, NoFocus)   -> (at (t - 1) rs ==)
                             (t, FocusAt i) -> (at (i + t) rs ==)
  where at i xs = xs !! (i `mod` length xs)
