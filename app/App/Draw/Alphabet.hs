module App.Draw.Alphabet where

  import Graphics.Gloss.Data.Picture
  -- |Draws a horizontal block of length 120 drawn from a given point.
  hB :: (Float, Float) -- ^ (x,y)-coordinates of the point
    -> Picture -- ^ the resulting picture
  hB (x, y) = polygon [(x-10, y+10), (x-10, y-10), (x + 110, y-10), (x+110, y+10)]

  -- |Draws a vertical block of length 120 drawn from a given point.
  vB :: (Float, Float) -- ^ (x,y)-coordinates of the point
    -> Picture -- ^ the resulting picture
  vB (x, y) = polygon [(x-10, y+10), (x+10, y+10), (x+10, y-110), (x-10, y-110)]

  -- |Draws a diagonal block between two given points.
  dB :: (Float, Float) -- ^ (x,y)-cordinates of the first point
    -> (Float, Float) -- ^ (x,y)-cordinates of the second point
    -> Picture -- ^ the resulting picture
  dB (x1, y1) (x2, y2) = polygon [(x1-10, y1), (x1+10, y1), (x2-10, y2), (x2+10, y2)]

  -- |A picture of letter S in a 120x220 box.
  aS :: Picture
  aS = pictures [hB (-100, 0), hB (-100, -100), hB (-100, -200), vB (-100, 0), vB (0, -100)]

  -- |A picture of letter T in a 120x220 box.
  aT :: Picture
  aT = pictures [hB (-100, 0), vB (-50, 0), vB (-50, -100)]

  -- |A picture of letter A in a 120x220 box.
  aA :: Picture
  aA = pictures [hB (-100, 0), hB (-100, -100), vB (-100, 0), vB (-100, -100), vB (0, 0), vB (0, -100)]

  -- |A picture of letter R in a 120x220 box.
  aR :: Picture
  aR = pictures [hB (-100, 0), hB (-100, -100), vB (-100, 0), vB (0, 0), vB (-100, -100), dB (-100, -100) (0, -200)]

  -- |A picture of letter P in a 120x220 box.
  aP :: Picture
  aP = pictures [hB (-100, 0), hB (-100, -100), vB (-100, 0), vB (-100, -100), vB (0, 0)]

  -- |A picture of letter O in a 120x220 box.
  aO :: Picture
  aO = pictures [hB (-100, 0), hB (-100, -200), vB (-100, 0), vB (-100, -100), vB (0, 0), vB (0, -100)]

  -- |A picture of letter N in a 120x220 box.
  aN :: Picture
  aN = pictures [vB (-100, 0), vB (-100, -100), vB (0, 0), vB (0, -100), dB (-100, 0) (0, -200)]

  -- |A picture of letter G in a 120x220 box.
  aG :: Picture
  aG = pictures [hB (-100, 0), hB (-100, -200), scale 0.5 1 . hB $ (-50, -100), vB (-100, 0), vB (-100, -100), vB (0, -100)]

  -- |A picture of an exclamation mark in a 120x220 box.
  aEx :: Picture
  aEx = pictures [polygon [(-110, -190), (-90, -190), (-90, -210), (-110, -210)], scale 1 0.8 . vB $ (-100, 0)]

  -- |A picture of letter U in a 120x220 box.
  aU :: Picture
  aU = pictures [hB (-100, -200), vB (-100, 0), vB (-100, -100), vB (0, 0), vB (0, -100)]

  -- |A picture of letter E in a 120x220 box.
  aE :: Picture
  aE = pictures [hB (-100, 0), hB (-100, -100), hB (-100, -200), vB (-100, 0), vB (-100, -100)]
