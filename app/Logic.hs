module Logic where

  import Engine
  import Graphics.Gloss.Interface.IO.Game
  import Data.List.Split
  import System.Directory

  type Record = String

  recordUncurry :: Record -> (Float, Int)
  recordUncurry r = (time, score) where
    records = splitOn " " r
    undone = map (splitOn ":") records
    time :: Float
    score :: Int
    time = 60*(read ((head undone) !! 0)) + read ((head undone) !! 1) + (read ((head undone) !! 2))/100
    score = read . last . last $ undone

  paddleAngle :: Float -> Float -> Float
  paddleAngle paddle ball = (pi/8)*(1 + (ball - paddle)/50)/2 + 7*(pi/8)*(1 -(1 + (ball - paddle)/50)/2)

  normalize :: Float -> Float
  normalize f = max 11 . min 789 $ f

  moveBall :: Float -> World -> World
  moveBall f w = w {game = (game w) {dir = dirn, ball = (xt, yt)}, time = (time w) + f}
   where
    g = game w
    xn = fst (ball g) + f * vel g * cos (dir g)
    yn = snd (ball g) + f * vel g * sin (dir g)
    dirx
      |(max 10 $ min 790 xn) == xn = 1
      |otherwise = -1
    diry
      |(max 10 $ min 790 yn) == yn = 1
      |otherwise = -1
    dirn
      |dirx == -1 = pi - diry * dir g
      |(diry == -1) && (dirx == 1) = diry * dir g
      |max (p1 g - 50) (min (p1 g + 50) xn) == xn && (yn >= 770) = - paddleAngle (p1 g) xn
      |max (p2 g - 50) (min (p2 g + 50) xn) == xn && (yn <= 30) = paddleAngle (p2 g) xn
      |otherwise = (dir g)
    xt = (f * vel g * cos dirn) + normalize (fst (ball g))
    yt = (f * vel g * sin dirn) + normalize (snd (ball g))

  scoreCheck :: World -> World
  scoreCheck world
    |(snd . ball . game $ world) <=15 = world {game = (Game (p1 . game $ world) 400 (400, 400) 0.72 ((vel . game $ world) + 30)), p1Score = (p1Score world)+1}
    |(snd . ball . game $ world) >=785 = world {game = (Game (p1 . game $ world) 400 (400, 400) 0.72 (vel . game $ world)), p2Score = (p2Score world)+1}
    |otherwise = world

  aiMove :: World -> World
  aiMove world = world {game = (game world) {p2 = p2Bounded}}
    where
      dist = p2 (game world) - fst (ball (game world))
      p2Unbounded
        |(dist > 2) = p2 (game world) - 0.75*((fromIntegral (p1Score world)) + 1)
        |(dist < -2) = p2 (game world) + 0.75*((fromIntegral (p1Score world)) + 1)
        |otherwise = p2 (game world)
      p2Bounded = max 50 . min 750 $ p2Unbounded

  gameOver :: World -> World
  gameOver w
    |(p1Score w) == 7 = w {result = Player}
    |(p2Score w) == 7 = w {result = AI}
    |otherwise = w

  updateWorld :: Float -> World -> IO World
  updateWorld seconds world@(World _ _ _ Ongoing _ _) = do
      return $ gameOver . scoreCheck . aiMove $ moveBall seconds world
  updateWorld seconds world = do
      return $ world {idleTime = (idleTime world) + seconds}

  eventHandler :: Event -> World -> IO World
  eventHandler (EventMotion (x,y)) world =  do
    let g = game world
    let g1 = g {p1 = max 50 . min 750 $ (y+400)}
    return $ world {game = g1}
  eventHandler (EventKey (Char 'r') Up _ _) world = do
    return $ startWorld {game = (game world) {p1 = p1 $ game world, vel = 300}, result = Ongoing, time = 0}
  eventHandler (EventKey (Char 'p') Down _ _) world = do
    return $ world {result = Idle}
  eventHandler (EventKey (Char 's') Down _ _) world
    |((result world == Player) || (result world == AI)) = do
      currDir <- getCurrentDirectory
      let file = currDir ++ "/records"
      appendFile file (timeAsText world ++ " " ++ (scoreAsText world) ++ "\n")
      return world
    |otherwise = do
      return world
  eventHandler _ world = do
    return world

  timeAsText :: World -> String
  timeAsText w = timeText where
    t = time w
    minutes = floor (t/60)
    seconds = floor (t - 60 * (fromIntegral (floor (t/60))))
    rest =  floor . (100 * ) . snd . properFraction $ t
    timeText = (show minutes) ++ ":" ++ (show seconds) ++ ":" ++ (show rest)

  scoreAsText :: World -> String
  scoreAsText world = score where
    s1 = show (p1Score world)
    s2 = show (p2Score world)
    score = s1 ++ ":" ++ s2
