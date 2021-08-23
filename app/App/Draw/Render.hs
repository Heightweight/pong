module App.Draw.Render where

  import App.Logic (recordUncurry, Record, scoreAsText, timeAsText, World(..), Game(..), Result(..))
  import App.Draw.Alphabet
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

  idle :: Color -> IO Picture
  idle c = do
    let pongPicture = color c . translate (-160) (100) . pictures $ zipWith (flip translate 0) (map (150*) [0..4]) [aP, aO, aN, aG, aEx]
    let width = 150
    let stripe = polygon [(-400 + width, -400), (-400, -400), (-400, -400 + width), (400 - width, 400), (400, 400), (400, 400 - width)]
    return $ pictures [stripe,  rotate (-45) . scale 0.8 0.8 $ pongPicture]

  pause :: Color -> IO Picture
  pause c = do
    let pausePicture = color c . translate (-160) (100) . pictures $ zipWith (flip translate 0) (map (150*) [0..4]) [aP, aA, aU, aS, aE]
    let width = 150
    let stripe = polygon [(-400 + width, -400), (-400, -400), (-400, -400 + width), (400 - width, 400), (400, 400), (400, 400 - width)]
    return $ pictures [stripe,  rotate (-45) . scale 0.8 0.8 $ pausePicture]

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
    Paused -> do
      let g = game world
      front <- pause . greyN $ (1 - 2 * abs (0.5 - 2 * snd (properFraction (idleTime world))))
      return $ color white $ pictures [(rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g]), translate (-400) (-400) . text . scoreAsText $ world, displayTime world, front]
    Idle -> do
      let g = game world
      front <- idle . greyN $ (1 - 2 * abs (0.5 - 2 * snd (properFraction (idleTime world))))
      return $ color white $ pictures [(rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g]), translate (-400) (-400) . text . scoreAsText $ world, displayTime world, front]
