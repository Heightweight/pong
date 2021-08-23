module App.Draw.Alphabet where

  import Graphics.Gloss.Data.Picture

  hB :: (Float, Float) -> Picture
  hB (x, y) = polygon [(x-10, y+10), (x-10, y-10), (x + 110, y-10), (x+110, y+10)]

  vB :: (Float, Float) -> Picture
  vB (x, y) = polygon [(x-10, y+10), (x+10, y+10), (x+10, y-110), (x-10, y-110)]

  dB :: (Float, Float) -> (Float, Float) -> Picture
  dB (x1, y1) (x2, y2) = polygon [(x1-10, y1), (x1+10, y1), (x2-10, y2), (x2+10, y2)]

  aS :: Picture
  aS = pictures [hB (-100, 0), hB (-100, -100), hB (-100, -200), vB (-100, 0), vB (0, -100)]

  aT :: Picture
  aT = pictures [hB (-100, 0), vB (-50, 0), vB (-50, -100)]

  aA :: Picture
  aA = pictures [hB (-100, 0), hB (-100, -100), vB (-100, 0), vB (-100, -100), vB (0, 0), vB (0, -100)]

  aR :: Picture
  aR = pictures [hB (-100, 0), hB (-100, -100), vB (-100, 0), vB (0, 0), vB (-100, -100), dB (-100, -100) (0, -200)]

  aP :: Picture
  aP = pictures [hB (-100, 0), hB (-100, -100), vB (-100, 0), vB (-100, -100), vB (0, 0)]

  aO :: Picture
  aO = pictures [hB (-100, 0), hB (-100, -200), vB (-100, 0), vB (-100, -100), vB (0, 0), vB (0, -100)]

  aN :: Picture
  aN = pictures [vB (-100, 0), vB (-100, -100), vB (0, 0), vB (0, -100), dB (-100, 0) (0, -200)]

  aG :: Picture
  aG = pictures [hB (-100, 0), hB (-100, -200), scale 0.5 1 . hB $ (-50, -100), vB (-100, 0), vB (-100, -100), vB (0, -100)]

  aEx :: Picture
  aEx = pictures [polygon [(-110, -190), (-90, -190), (-90, -210), (-110, -210)], scale 1 0.8 . vB $ (-100, 0)]

  aU :: Picture
  aU = pictures [hB (-100, -200), vB (-100, 0), vB (-100, -100), vB (0, 0), vB (0, -100)]

  aE :: Picture
  aE = pictures [hB (-100, 0), hB (-100, -100), hB (-100, -200), vB (-100, 0), vB (-100, -100)]
