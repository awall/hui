module Hui.Hairo where 

import Hui.Geometry
import Hui.Color

import Data.Text
import Control.Monad.Reader(ask, liftIO)
import qualified Graphics.Rendering.Cairo as Cairo
import Foreign.Ptr (Ptr)
import Graphics.Rendering.Cairo.Internal(Cairo,unCairo)

type Draw = Cairo.Render ()

liftRender0 :: (Cairo -> IO a) -> Cairo.Render a
liftRender0 f = ask >>= \context -> liftIO (f context)

foreign import ccall "cairo.h cairo_new_sub_path"
  cairo_new_sub_path :: Ptr Cairo -> IO ()

newSubPath :: Cairo.Render ()
newSubPath = liftRender0 $ cairo_new_sub_path . unCairo

showText :: Text -> Draw
showText = Cairo.showText

save :: Draw
save = Cairo.save

restore :: Draw
restore = Cairo.restore

rectangle :: Rect -> Draw
rectangle (Rect (XY l t) (XY r b)) = Cairo.rectangle l t (r-l) (b-t)

clip :: Draw
clip = Cairo.clip

setSourceRGBA :: Color -> Draw
setSourceRGBA (Color r g b a) = Cairo.setSourceRGBA r g b a

fill :: Draw
fill = Cairo.fill

stroke :: Draw
stroke = Cairo.stroke

translate :: XY -> Draw
translate (XY x y) = Cairo.translate x y

moveTo :: XY -> Draw
moveTo (XY x y) = Cairo.moveTo x y

lineTo :: XY -> Draw
lineTo (XY x y) = Cairo.lineTo x y

scale :: XY -> Draw
scale (XY x y) = Cairo.scale x y

arc :: XY -> Double -> Double -> Double -> Draw
arc (XY x y) = Cairo.arc x y

closePath :: Draw
closePath = Cairo.closePath

setLineWidth :: Double -> Draw
setLineWidth = Cairo.setLineWidth

withLinearPattern :: XY -> XY -> (Cairo.Pattern -> Draw) -> Draw
withLinearPattern (XY x1 y1) (XY x2 y2) = Cairo.withLinearPattern x1 y1 x2 y2

patternAddColorStopRGBA :: Cairo.Pattern -> Double -> Color -> Draw
patternAddColorStopRGBA pattern x (Color r g b a) = Cairo.patternAddColorStopRGBA pattern x r g b a

setSource :: Cairo.Pattern -> Draw
setSource = Cairo.setSource
