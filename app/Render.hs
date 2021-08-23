module Render where

  import Engine
  import Logic (recordUncurry, Record, scoreAsText, timeAsText)
  import Graphics.Gloss.Data.Picture
  import Graphics.Gloss.Data.Color
  import System.Directory (getCurrentDirectory)
  import Data.List (nub, sortOn)
  import Data.List.Split (splitOn)
  import Data.Tuple (swap)

  playerDraw :: Int -> Float -> Picture
  playerDraw 1 x = Polygon [(x-50, 780), (x+50, 780), (x+50, 800), (x-50, 800)]
  playerDraw _ x = Polygon [(x-50, 20), (x+50, 20), (x+50, 0), (x-50, 0)]

  ballDraw :: Point -> Picture
  ballDraw (x, y) = Translate x y (Circle 10)

  displayTime :: World -> Picture
  displayTime w = translate (0) (370) . scale 0.2 0.2 . text . timeAsText $ w

  victory :: World -> Picture
  victory world = pictures [translate (-350) 0 . text $ "You win!", translate 0 (-420) . displayTime $ world, translate (-200) (-50) . scale 0.2 0.2 . text $ "your time:"]

  defeat :: World -> Picture
  defeat world = pictures [translate (-350) 0 . text $ "You lose!", translate 0 (-420) . displayTime $ world, translate (-200) (-50) . scale 0.2 0.2 . text $ "your time:"]





  leaderboardSort :: [Record] -> [Record]
  leaderboardSort = sortOn (swap . recordUncurry)

  leaderboard :: IO Picture
  leaderboard = do
    currDir <- getCurrentDirectory
    let file = currDir ++ "/records"
    content <- readFile file
    let records = lines content
    let top10 = take 10 . nub . leaderboardSort $ records
    return $ pictures (zipWith ($) (map (\n -> translate 0 (n*(-110))) [1..10]) (map text top10))

  hBrick :: (Float, Float) -> Picture
  hBrick (x, y) = polygon [(x-10, y+10), (x-10, y-10), (x + 110, y-10), (x+110, y+10)]

  vBrick :: (Float, Float) -> Picture
  vBrick (x, y) = polygon [(x-10, y+10), (x+10, y+10), (x+10, y-110), (x-10, y-110)]

  dBrick :: (Float, Float) -> Picture
  dBrick (x, y) = polygon [(x, y+10), (x, y-10), (x+90, y-100), (x+110, y-100)]

  idle :: Color -> IO Picture
  idle c = do
    let s = pictures [hBrick (-100, 0), hBrick (-100, -100), hBrick (-100, -200), vBrick (-100, 0), vBrick (0, -100)]
    let t = pictures [hBrick (-100, 0), vBrick (-50, 0), vBrick (-50, -100)]
    let a = pictures [hBrick (-100, 0), hBrick (-100, -100), vBrick (-100, 0), vBrick (-100, -100), vBrick (0, 0), vBrick (0, -100)]
    let r = pictures [hBrick (-100, 0), hBrick (-100, -100), vBrick (-100, 0), vBrick (0, 0), vBrick (-100, -100), dBrick (-100, -100)]
    let start = color c . translate (-160) (100) . pictures $ zipWith (flip translate 0) (map (140*) [0..4]) [s, t, a, r, t]
    let width = 150
    let stripe = polygon [(-400 + width, -400), (-400, -400), (-400, -400 + width), (400 - width, 400), (400, 400), (400, 400 - width)]
    return $ pictures [stripe,  rotate (-45) . scale 0.8 0.8 $ start]

  layer :: Float -> Picture -> Picture
  layer n = pictures . zipWith ($) (map (\k -> translate k k)  [(-n)..n]) . replicate (floor n)

  worldDraw :: World -> IO Picture
  worldDraw world = case (result world) of
    Ongoing -> do
      let g = game world
      return $ color white $ pictures [(rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g]), translate (-400) (-400) . text . scoreAsText $ world, displayTime world]
    Player -> do
      leaders <- leaderboard
      return $ color white $ pictures [victory world, translate (-200) (-100) . scale 0.2 0.2 $ leaders]
    AI -> do
      leaders <- leaderboard
      return $ color white $ pictures [defeat world, translate (-200) (-100) . scale 0.2 0.2 $ leaders]
    Idle -> do
      let g = game world
      front <- idle . greyN . snd . properFraction . idleTime $ world
      return $ color white $ pictures [(rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g]), translate (-400) (-400) . text . scoreAsText $ world, displayTime world, front]