{-# LANGUAGE Arrows #-}
module DemoPaint where

import qualified Hui.Hairo as Hairo
import Hui.Auto
import Hui.Color
import Hui.Event
import qualified Hui.Geometry as G
import Hui.Gtk(mainLoop)

import Control.Arrow
import Data.Maybe
import qualified Data.Sequence as S
import Data.Traversable (mapM)

import Prelude hiding (mapM)

data Shape = Rectangle | Ellipse | Triangle deriving Eq
data Object = Object Shape G.Rect (Color, Color)
data Canvas = Canvas (S.Seq Object) (Maybe Object)

margin :: Double
margin = 6

main :: IO ()
main = mainLoop $ proc event -> do
  bounds <- windowBounds -< event
  let mainFrame = G.Frame margin bounds
  let [bottomFrame, canvasFrame] = G.subsBTT [66] mainFrame

  (paletteFrame, shape, shapesDraw) <- shapes  -< (bottomFrame,                      event)
  (fgbg, paletteDraw)               <- palette -< (paletteFrame,                     event)
  canvasDraw                        <- canvas  -< (G.inner canvasFrame, shape, fgbg, event)

  returnA -< do Hairo.setSourceRGBA gray
                Hairo.rectangle bounds
                Hairo.fill
                canvasDraw
                shapesDraw
                paletteDraw

canvas :: Auto (G.Rect, Shape, (Color, Color), Event) Hairo.Draw
canvas = proc params@(bounds, _, _, _) -> do
  (Canvas os o) <- accumA nextCanvas emptyCanvas -< params
  returnA -< do Hairo.save
                Hairo.rectangle bounds
                Hairo.clip
                Hairo.setSourceRGBA white
                Hairo.rectangle bounds
                Hairo.fill
                mapM objectDraw os
                mapM objectDraw o
                Hairo.restore

objectDraw :: Object -> Hairo.Draw
objectDraw (Object s r (fg,bg)) = do
  Hairo.setSourceRGBA (translucent bg)
  shapeDraw r s
  Hairo.fill
  Hairo.setSourceRGBA fg
  shapeDraw r s
  Hairo.setLineWidth 6
  Hairo.stroke

emptyCanvas :: Canvas
emptyCanvas = Canvas S.empty Nothing

nextCanvas :: Canvas -> (G.Rect, Shape, (Color, Color), Event) -> Canvas
nextCanvas (Canvas objects object) (bounds, selectedShape, selectedColors, event) = 
  case (event, object) of 
    (EMouseDown xy, Nothing)             | G.inside bounds xy -> Canvas objects (Just (obj xy xy))
    (EMouseDown xy, Just (Object _ r _)) | G.inside bounds xy -> Canvas (objects S.|> obj (G.lt r) xy) Nothing
    (EMouseMove xy, Just (Object _ r _))                      -> Canvas objects (Just (obj (G.lt r) xy))
    _                                                         -> Canvas objects object
    where obj a b = Object selectedShape (G.Rect a b) selectedColors

palette :: Auto (G.Frame, Event) ((Color, Color), Hairo.Draw)
palette = proc (frame, event) -> do
  let (G.XY _ h) = G.size (G.inner frame)
  let [leftFrame, rightFrame] = G.subsLTR [h] frame
  let fgRect = G.addLTRB 0 0 (-10) (-10) (G.inner leftFrame)
  let bgRect = G.addLTRB 10 10 0 0 (G.inner leftFrame)
  let [topFrame, botFrame] = G.splitV 2 rightFrame

  (_, fg, topDraw) <- colors black -< (topFrame, event)
  (_, bg, botDraw) <- colors white -< (botFrame, event)

  returnA -< ((fg, bg), do topDraw
                           botDraw
                           colorBoxDraw (bgRect, bg)
                           colorBoxDraw (fgRect, fg))

shapes :: Auto (G.Frame, Event) (G.Frame, Shape, Hairo.Draw)
shapes = selector shapeBoxDraw Rectangle [Rectangle, Ellipse, Triangle]

colors :: Color -> Auto (G.Frame, Event) (G.Frame, Color, Hairo.Draw)
colors initial = selector (const colorBoxDraw) initial [white, black, red, blue, green, yellow, cyan, magenta]

selector :: (a -> (G.Rect, a) -> Hairo.Draw) -> a -> [a] -> Auto (G.Frame, Event) (G.Frame, a, Hairo.Draw)
selector draw initial options = proc (frame, event) -> do
  let (G.XY _ h) = G.size (G.inner frame)
  let subs = G.subsLTR (map (const h) options) frame  
  let boxes = zip (map G.inner subs) options
  xy <- clicks -< event
  selected <- remember initial -< findInside boxes xy
  returnA -< (last subs, selected, mapM_ (draw selected) boxes)

findInside :: [(G.Rect, a)] -> G.XY -> Maybe a
findInside [] _ = Nothing
findInside ((r,a):ras) xy = if G.inside r xy then Just a else findInside ras xy

colorBoxDraw :: (G.Rect, Color) -> Hairo.Draw
colorBoxDraw (bounds, color) = do
  Hairo.setSourceRGBA black
  Hairo.rectangle bounds
  Hairo.stroke
  Hairo.setSourceRGBA color
  Hairo.rectangle bounds
  Hairo.fill

shapeBoxDraw :: Shape -> (G.Rect, Shape) -> Hairo.Draw
shapeBoxDraw selectedShape (bounds, shape) = do
    Hairo.rectangle bounds
    Hairo.setSourceRGBA black
    Hairo.stroke
    Hairo.withLinearPattern (G.lt bounds) (G.rb bounds) $ \pat -> do
      Hairo.patternAddColorStopRGBA pat 0.1 fg
      Hairo.patternAddColorStopRGBA pat 0.9 bg
      Hairo.setSource pat
      Hairo.rectangle bounds
      Hairo.fill
    let s = shapeDraw (G.addLTRB 10 10 (-10) (-10) bounds) shape
    Hairo.setSourceRGBA black >> s >> Hairo.stroke
    Hairo.setSourceRGBA white >> s >> Hairo.fill
    where (fg,bg) = if selectedShape == shape
                       then (dark yellow, orange)
                       else (gray, dark $ dark $ dark gray)

shapeDraw :: G.Rect -> Shape -> Hairo.Draw
shapeDraw bounds shape =
  if G.zero (G.size bounds)
     then return ()
     else case shape of Rectangle -> Hairo.rectangle bounds
                        Triangle  -> do Hairo.moveTo (G.lb bounds)
                                        Hairo.lineTo (G.relativeXY (G.XY 0.5 0) bounds)
                                        Hairo.lineTo (G.rb bounds)
                                        Hairo.closePath
                        Ellipse   -> do Hairo.save
                                        Hairo.translate (G.center bounds)
                                        Hairo.scale (G.size bounds)
                                        Hairo.newSubPath
                                        Hairo.arc (G.XY 0 0) 0.5 0 (2*pi)
                                        Hairo.closePath
                                        Hairo.restore
