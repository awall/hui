module Hui.Geometry where

import Data.Maybe

data XY = XY !Double !Double
  deriving Eq

data Rect = Rect !XY !XY
  deriving Eq

data Frame = Frame !Double !Rect
  deriving Eq

flipRect :: Rect -> Rect
flipRect (Rect (XY l t) (XY r b)) = Rect (XY (-r) (-b)) (XY (-l) (-t))

flipFrame :: Frame -> Frame
flipFrame (Frame w r) = Frame w (flipRect r)

transposeRect :: Rect -> Rect
transposeRect (Rect (XY l t) (XY r b)) = Rect (XY t l) (XY b r)

transposeFrame :: Frame -> Frame
transposeFrame (Frame w r) = Frame w (transposeRect r)

inner :: Frame -> Rect
inner (Frame m (Rect (XY l t) (XY r b))) = Rect (XY (l+m) (t+m)) (XY (r-m) (b-m))

outer :: Frame -> Rect
outer (Frame _ r) = r

subsLTR :: [Double] -> Frame -> [Frame]
subsLTR w (Frame m (Rect (XY l t) (XY r b))) =
  [ Frame m (Rect (XY il t) (XY ir b)) | (il,ir) <- zip ls rs]
  where ws = map (+2*m) w
        ls = scanl (\a b -> a+b-m) l ws
        rs = map (uncurry (+)) (zip ls ws) ++ [r]

subsRTL :: [Double] -> Frame -> [Frame]
subsRTL w f = map flipFrame (subsLTR w (flipFrame f))

subsTTB :: [Double] -> Frame -> [Frame]
subsTTB w f = reverse $ map transposeFrame (subsLTR w (transposeFrame f))

subsBTT :: [Double] -> Frame -> [Frame]
subsBTT w f = reverse $ map flipFrame (subsTTB w (flipFrame f))

splitH :: Int -> Frame -> [Frame]
splitH n f@(Frame m (Rect (XY l _) (XY r _))) =
  take n (subsRTL (replicate n w) f)
  where w  = (r - l - m - (fromIntegral n)*m) / (fromIntegral n)

splitV :: Int -> Frame -> [Frame]
splitV n f = reverse $ map transposeFrame (splitH n (transposeFrame f))

size :: Rect -> XY
size (Rect (XY l t) (XY r b)) = XY (r-l) (b-t)

translate :: XY -> Rect -> Rect
translate (XY x y) = addLTRB x y x y

cascade :: XY -> [XY]
cascade (XY w h) = map (\i -> XY (i*w) (i*h)) [0..]

at :: XY -> XY -> Rect
at (XY w h) (XY x y) = Rect (XY x y) (XY (x+w) (y+h))

zero :: XY -> Bool
zero (XY 0 _) = True
zero (XY _ 0) = True
zero (XY _ _) = False

lt :: Rect -> XY
lt (Rect xy _) = xy

rb :: Rect -> XY
rb (Rect _ xy) = xy

lb :: Rect -> XY
lb (Rect (XY l _) (XY _ b)) = XY l b

rt :: Rect -> XY
rt (Rect (XY _ r) (XY t _)) = XY r t

relativeXY :: XY -> Rect -> XY
relativeXY (XY x y) (Rect (XY l t) (XY r b)) = XY (l+(x*(r-l))) (t+(y*(b-t)))

center :: Rect -> XY
center = relativeXY (XY 0.5 0.5)

addLTRB :: Double -> Double -> Double -> Double -> Rect -> Rect
addLTRB dl dt dr db (Rect (XY l t) (XY r b)) =
  Rect (XY (l+dl) (t+dt)) (XY (r+dr) (b+db))

setLTRB :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Rect -> Rect
setLTRB ml mt mr mb (Rect (XY l t) (XY r b)) =
  Rect (XY (fromMaybe l ml) (fromMaybe t mt)) (XY (fromMaybe r mr) (fromMaybe b mb))

inside :: Rect -> XY -> Bool
inside (Rect (XY l t) (XY r b)) (XY x y) =
  x `between` (l,r) && y `between` (t,b)
  where between a (mn,mx) = a >= mn && a < mx
