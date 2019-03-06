module Hui.Color where

data Color = Color Double Double Double Double -- R G B A

white   = Color 1.0 1.0 1.0 1.0
black   = Color 0.0 0.0 0.0 1.0
red     = Color 1.0 0.0 0.0 1.0
blue    = Color 0.0 0.0 1.0 1.0
green   = Color 0.0 1.0 0.0 1.0
yellow  = Color 1.0 1.0 0.0 1.0
cyan    = Color 0.0 1.0 1.0 1.0
magenta = Color 1.0 0.0 1.0 1.0
gray    = Color 0.5 0.5 0.5 1.0
orange  = Color 0.8 0.6 0.0 1.0

dark :: Color -> Color
dark (Color r g b a) =
  let sub x = max 0 (x - 0.1) in Color (sub r) (sub g) (sub b) a

translucent :: Color -> Color
translucent (Color r g b a) = Color r g b (0.7 * a)
