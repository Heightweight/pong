module App.Draw.Render where

  import App.Logic (recordUncurry, Record, scoreAsText, timeAsText, World(..), Game(..), Result(..))
  import App.Draw.Alphabet
  import Graphics.Gloss.Data.Picture
  import Graphics.Gloss.Data.Color
  import System.Directory (getCurrentDirectory, doesFileExist)
  import Data.List (nub, sortOn)
  import Data.List.Split (splitOn)
  import Data.Tuple (swap)

  -- | Draws a paddle for a specified player according to its position.
  playerDraw :: Int -- ^ the number of the paddle. (1 for the player paddle, anything else for the AI paddle)
    -> Float -- ^ the x-coordinate of the center of the paddle
    -> Float -- ^ a number used to generate the inner color of the paddle
    -> Picture -- ^ the resulting picture
  playerDraw 1 x t = pictures [polygon [(x-50, 780), (x+50, 780), (x+50, 800), (x-50, 800)], color newColor $ polygon [(x-45, 785), (x+45, 785), (x+45, 795), (x-45, 795)]] where
    scale = snd (properFraction t)
    newColor = bright $ mixColors scale (1-scale) blue green
  playerDraw _ x t = pictures [polygon [(x-50, 20), (x+50, 20), (x+50, 0), (x-50, 0)], color newColor $ polygon [(x-45, 15), (x+45, 15), (x+45, 5), (x-45, 5)]] where
    scale = snd (properFraction t)
    newColor = bright $ mixColors scale (1-scale) blue green

  -- | Draws the ball according to its position.
  ballDraw :: Point -- ^ the (x,y)-coordinates of the ball
    -> Float -- ^ a number used to generate the inner color of the ball
    -> Picture -- ^ the resulting picture
  ballDraw (x, y) t = translate x y $ pictures [(circleSolid 10), (color newColor $ circleSolid 5)] where
    scale = snd (properFraction t)
    newColor = bright $ mixColors scale (1-scale) blue green

  -- | Draws the game time of a world.
  displayTime :: Float -- ^ a number used to generate the color of the text
    -> World -- ^ the current world
    -> Picture -- ^ the resulting picture
  displayTime t w = color newColor . translate (0) (370) . scale 0.2 0.2 . text . timeAsText $ w where
    newColor = greyN $ (1 - 2 * abs (0.5 - 2 * snd (properFraction t)))

  -- | Draws a victory screen for a given world.
  -- Displays the total time of the game.
  victory :: Float -- ^ a number used to generate the color of the text
    -> World -- ^ the current world
    -> Picture -- ^ the resulting picture
  victory t world = pictures [translate (-350) 0 . text $ "You win!", translate 0 (-420) . displayTime t $ world, translate (-200) (-50) . scale 0.2 0.2 . text $ "your time:"]

  -- | Draws a defeat screen for a given world.
  -- Displayes the total time of the game.
  defeat :: Float -- ^ a number used to generate the color of the text
    -> World -- ^ the current world
    -> Picture -- ^ the resulting picture
  defeat t world = pictures [translate (-350) 0 . text $ "You lose!", translate 0 (-420) . displayTime t $ world, translate (-200) (-50) . scale 0.2 0.2 . text $ "your time:"]

  -- | Sorts a list of game records.
  leaderboardSort :: [Record] -> [Record]
  leaderboardSort = sortOn (\r -> (((-1) * (snd . recordUncurry $ r)), ((fst . recordUncurry $ r))))

  -- | The top-10 scores of all time as a picture wrapped in IO context.
  leaderboard :: IO Picture
  leaderboard = do
    currDir <- getCurrentDirectory
    let file = currDir ++ "/records"
    exists <- doesFileExist file
    content <- if exists
      then readFile file
      else return ""
    let records = lines content
    let top10 = take 10 . nub . leaderboardSort $ records
    let ld = "leaderboard:"
    return $ pictures (zipWith ($) (map (\n -> translate 0 (n*(-110))) [1..11]) (map text (ld:top10)))

  -- | Returns the first element of a quadruple
  fst4 :: (a, a, a, a) -- ^ the quadraple
    -> a -- its first element
  fst4 (a, _, _, _) = a

  -- | Generates a tiling for the idle/pause screen.
  borderTiling :: Float -- ^ current time
    -> Picture -- ^ the resulting tiling
  borderTiling f = pictures [rotate (-180) tilingTranslate, tilingTranslate] where
    tile = polygon [(0, 0), (40, 40), (90, 40), (50, 0)]
    tiling = pictures $ zipWith (flip translate 0) (map (100*) [(-10)..10]) $ replicate 21 tile
    t = snd (properFraction f)
    tilingTranslate = translate 0 150 . rotate (-45) . translate (100 * t) 0 $ tiling

  -- | Generates an idle-screen wrapped in IO context with a given   color.
  idle :: Color -- ^ the color of the title text
    -> IO Picture -- ^ the resulting picture to be used in an IO context.
  idle c = do
    let pongPicture = color c . translate (-160) (100) . pictures $ zipWith (flip translate 0) (map (150*) [0..4]) [aP, aO, aN, aG, aEx]
    let width = 150
    let stripe = polygon [(-400 + width, -400), (-400, -400), (-400, -400 + width), (400 - width, 400), (400, 400), (400, 400 - width)]
    let prompt = translate 0 (-50) . rotate (-45) . scale 0.2 0.2 . text $ "press 'r' to start"
    return $ pictures [stripe,  rotate (-45) . scale 0.8 0.8 $ pongPicture, translate 0 (-200) prompt]

  -- | Generates a pause-screen wrapped in IO context with a given color.
  pause :: Color -- ^ the color of the title text
    -> IO Picture -- ^ the resulting picture to be used in an IO context.
  pause c = do
    let pausePicture = color c . translate (-160) (100) . pictures $ zipWith (flip translate 0) (map (150*) [0..4]) [aP, aA, aU, aS, aE]
    let width = 150
    let stripe = polygon [(-400 + width, -400), (-400, -400), (-400, -400 + width), (400 - width, 400), (400, 400), (400, 400 - width)]
    let prompt = translate 0 (-50) . rotate (-45) . scale 0.2 0.2 . text $ "press 'p' to continue"
    return $ pictures [stripe,  rotate (-45) . scale 0.8 0.8 $ pausePicture, translate 0 (-200) prompt]

  -- | A picture of a world wrapped in IO context.
  worldDraw :: World -- ^ the current world
    -> IO Picture -- ^ the resulting picture to be used in an IO context.
  worldDraw world = case (result world) of
    Ongoing -> do
      let g = game world
      let t = time world
      return $ color white $ pictures [(rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 (p1 g) t, playerDraw 2 (p2 g) t, ballDraw (ball g) t]), translate (-400) (-400) . text . scoreAsText $ world, displayTime 0.25 world]
    Player -> do
      let t = idleTime world
      leaders <- leaderboard
      let newColor = greyN $ (1 - 2 * abs (0.5 - 2 * snd (properFraction t)))
      let prompt = color newColor . translate (-200) (-80) . scale 0.2 0.2 . text $  "press 'r' to restart, 's' to save"
      return $ color white $ pictures [prompt, victory t world, translate (-200) (-100) . scale 0.2 0.2 $ leaders]
    AI -> do
      let t = idleTime world
      leaders <- leaderboard
      let newColor = greyN $ (1 - 2 * abs (0.5 - 2 * snd (properFraction t)))
      let prompt = color newColor . translate (-200) (-80) . scale 0.2 0.2 . text $  "press 'r' to restart, 's' to save"
      return $ color white $ pictures [prompt, defeat t world, translate (-200) (-100) . scale 0.2 0.2 $ leaders]
    Paused -> do
      let g = game world
      front <- pause . greyN $ (1 - 2 * abs (0.5 - 2 * snd (properFraction (idleTime world))))
      let t = idleTime world
      let tilingUpper = borderTiling t
      return $ color white $ pictures [tilingUpper, (rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 (p1 g) t, playerDraw 2 (p2 g) t, ballDraw (ball g) t]), translate (-400) (-400) . text . scoreAsText $ world, displayTime 0.25 world, front]
    Idle -> do
      let g = game world
      let t = idleTime world
      let tilingUpper = borderTiling t
      front <- idle . greyN $ (1 - 2 * abs (0.5 - 2 * snd (properFraction (idleTime world))))
      return $ color white $ pictures [tilingUpper, (rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 (p1 g) t, playerDraw 2 (p2 g) t, ballDraw (ball g) t]), translate (-400) (-400) . text . scoreAsText $ world, displayTime 0.25 world, front]
