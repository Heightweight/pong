module App.Logic where

  import Graphics.Gloss.Interface.IO.Game
  import Graphics.Gloss.Data.Point (Point)
  import Data.List.Split (splitOn)
  import System.Directory (getCurrentDirectory)
  import Control.Monad (guard)

  -- | A game value. It holds all necessary data to calculate the next game state depending on time.
  data Game = Game {
  -- | x-coordinate of the player-controlled paddle
  p1 :: Float,
  -- | x-coordinate of the computer-controlled paddle
  p2 :: Float,
  -- | the coordinates of the ball
  ball :: Point,
  -- | the direction in which the ball is moving in radians
  dir :: Float,
  -- | the velocity of the ball
  vel :: Float
  } deriving (Show)


  -- | Describes the result and/or state of the current world.
  data Result
    -- | for games in progress
    = Ongoing
    -- | for finished games won by the player
    | Player
    -- | for finished games won by the AI
    | AI
    -- | for games that have not been started yet
    | Idle
    -- | for paused games
    | Paused deriving (Eq, Show)

  -- | Describes the world of the game. Holds the game data and the score, time and result of the game.
  data World = World {
  -- | the current game state
  game :: Game,
  -- | player's score
  p1Score :: Int,
  -- | AI's score
  p2Score :: Int,
  -- | the result/state of the current game
  result :: Result,
  -- | the playtime (only includes active playing)
  time :: Float,
  -- | the time of inactivity (only when the game is not being played)
  idleTime :: Float
  } deriving (Show)
  -- | Used for storing the time and result of a finished game.
  type Record = String

  -- | The default world of the game. By default the game has not been started yet, the paddles and the ball are centered,
  -- and the ball direction is 0.72 (in radians)
  startWorld :: World
  startWorld = (World (Game 400 400 (400, 400) 0.72 300) 0 0 Idle 0 0)

  -- | Returns a pair of time in seconds and the player's score of a given record
  recordUncurry :: Record -- ^ the record string (minutes:seconds:miliseconds player:ai)
    -> (Float, Int) -- ^ the resulting time-player pair
  recordUncurry r = (time, score) where
    records = splitOn " " r
    undone = map (splitOn ":") records
    time :: Float
    score :: Int
    time = 60*(read ((head undone) !! 0)) + read ((head undone) !! 1) + (read ((head undone) !! 2))/100
    score = read . head . last $ undone

  -- | Returns the bounce angle of a ball hitting a paddle.
  paddleAngle :: Float -- ^ the x-coordinate of the paddle
    -> Float -- ^ the x-coordinate of the ball
    -> Float -- ^ the resulting angle in radians
  paddleAngle paddle ball = (pi/8)*(1 + (ball - paddle)/50)/2 + 7*(pi/8)*(1 -(1 + (ball - paddle)/50)/2)

  -- | Returns a normalized coordinate for collision-avoidance purposes.
  normalize :: Float -- ^ the coordinate to be normalized
    -> Float -- ^ the resulting coordinate
  normalize f = max 11 . min 789 $ f

  -- | Checks for collision on the x-axis and changes the direction of the ball.
  collisionX :: Float -- ^ time passed in seconds
    -> World -- ^ the current world
    -> World -- ^ the updated world
  collisionX f w = w {game = g {dir = dirn}} where
    g = game w
    xn = fst (ball g) + f * vel g * cos (dir g)
    dirn
      |(max 10 $ min 790 xn) == xn = dir g
      |otherwise = pi - dir g
  -- | Checks for collision on the y-axis and changes the direction of the ball.
  collisionY :: Float -- ^ time passed in seconds
    -> World -- ^ the current world
    -> World -- ^ the updated world
  collisionY f w = w {game = (game w) {dir = dirn}} where
    g = game w
    yn = snd (ball g) + f * vel g * sin (dir g)
    dirn
      |(max 10 $ min 790 yn) == yn = dir g
      |otherwise = - dir g

  -- | Checks for collision with the player paddle and changes the direction of the ball.
  collisionPlayerPaddle :: Float -- ^ time passed in seconds
    -> World -- ^ the current world
    -> World -- ^ the updated world
  collisionPlayerPaddle f w = w {game = g {dir = dirn}} where
    g = game w
    xn = fst (ball g) + f * vel g * cos (dir g)
    yn = snd (ball g) + f * vel g * sin (dir g)
    dirn
      |max (p1 g - 50) (min (p1 g + 50) xn) == xn && (yn >= 770) = - paddleAngle (p1 g) xn
      |otherwise = dir g
  -- | Checks for collision with the AI paddle and changes the direction of the ball.
  collisionAIPaddle :: Float -- ^ time passed
    -> World -- ^ the current world
    -> World -- ^ the updated world
  collisionAIPaddle f w = w {game = g {dir = dirn}} where
    g = game w
    xn = fst (ball g) + f * vel g * cos (dir g)
    yn = snd (ball g) + f * vel g * sin (dir g)
    dirn
      |max (p2 g - 50) (min (p2 g + 50) xn) == xn && (yn <= 30) = paddleAngle (p2 g) xn
      |otherwise = dir g

  -- Changes the coordinates of the ball do get rid of any collision.
  popBall :: World -- ^ the current world
    -> World -- ^ the updated world
  popBall w = w {game = (game w) {ball = (normalize(fst . ball . game $ w), normalize(snd . ball . game $ w))}}

  -- Upticks the time by the amount passed.
  tickTime :: Float -- ^ time passed in seconds
    -> World -- ^ the current world
    -> World -- ^ the updated world
  tickTime t w = w {time = (time w) + t}

  -- | Moves the ball in a set direction for a given time accounting for collisions.
  moveBall :: Float -- ^ time passed in seconds
    -> World -- ^ the world to update
    -> World -- ^ the resulting world
  moveBall f w = w {game = (game w) {ball = (xt, yt)}}
   where
    g = game w
    xt = (f * vel g * cos (dir g)) + normalize (fst (ball g))
    yt = (f * vel g * sin (dir g)) + normalize (snd (ball g))

  -- | Changes the score and the game state if any player scores.
  -- Also changes the speed of the ball for each point scored by the player.
  scoreCheck :: World -- ^ the current world
    -> World -- ^ the world with updated score/field in case someone scored
  scoreCheck world
    |(snd . ball . game $ world) <=15 = world {game = (Game (p1 . game $ world) 400 (400, 400) 0.72 ((vel . game $ world) + 30)), p1Score = (p1Score world)+1}
    |(snd . ball . game $ world) >=785 = world {game = (Game (p1 . game $ world) 400 (400, 400) 0.72 (vel . game $ world)), p2Score = (p2Score world)+1}
    |otherwise = world

  -- | Moves the second player's paddle according to where the ball is.
  -- The bigger the player's score, the faster the paddle moves.
  aiMove :: World -- ^ the current world
    -> World -- ^ the world with updated paddle position
  aiMove world = world {game = (game world) {p2 = p2Bounded}}
    where
      dist = p2 (game world) - fst (ball (game world))
      p2Unbounded
        |(dist > 2) = p2 (game world) - 0.75*((fromIntegral (p1Score world)) + 1)
        |(dist < -2) = p2 (game world) + 0.75*((fromIntegral (p1Score world)) + 1)
        |otherwise = p2 (game world)
      p2Bounded = max 50 . min 750 $ p2Unbounded

  -- | Checks if the game is over and changes the result accordingly.
  -- The game ends once someone has scored 7 points.
  gameOver :: World -- ^ the current world
    -> World -- ^ the world with updated result.
  gameOver w
    |(p1Score w) == 7 = w {result = Player}
    |(p2Score w) == 7 = w {result = AI}
    |otherwise = w

  -- | Updates the world according to the time passed.
  updateWorld :: Float -- ^ time passed in seconds
    -> World -- ^ the current world
    -> IO World -- ^ the updated world wrapped in IO context
  updateWorld s w@(World _ _ _ Ongoing _ _) = do
      return $ gameOver
        . scoreCheck
        . aiMove
        . (moveBall s)
        . (tickTime s)
        . (popBall)
        . (collisionAIPaddle s)
        . (collisionPlayerPaddle s)
        . (collisionX s)
        . (collisionY s) $ w
  updateWorld s w = do
      return $ w {idleTime = (idleTime w) + s}

  -- | Updates the world according to an input event.
  eventHandler :: Event -- ^ the event to be processed
    -> World -- ^ the current world
    -> IO World -- ^ the updated world wrapped in IO context
  eventHandler (EventMotion (x,y)) world@(World _ _ _ Ongoing _ _) = do
    let g = game world
    let g1 = g {p1 = max 50 . min 750 $ (y+400)}
    return $ world {game = g1}
  eventHandler (EventKey (Char 'r') Up _ _) world = do
    return $ startWorld {game = (game world) {p1 = p1 $ game world, vel = 300}, result = Ongoing, time = 0}
  eventHandler (EventKey (Char 'p') Up _ _) world@(World _ _ _ Ongoing _ _) = do
    return $ world {result = Paused}
  eventHandler (EventKey (Char 'p') Up _ _) world@(World _ _ _ Paused _ _) = do
    return $ world {result = Ongoing}
  eventHandler (EventKey (Char 's') Down _ _) world
    |((result world == Player) || (result world == AI)) = do
      currDir <- getCurrentDirectory
      let file = currDir ++ "/records"
      appendFile file (timeAsText world ++ " " ++ (scoreAsText world) ++ "\n")
      return world
    |otherwise = do
      return world
  eventHandler _ w = do
    return w

  -- | Returns the time of the game in a (minutes:seconds:miliseconds) format.
  timeAsText :: World -- ^ the current world
    -> String -- ^ the time of active playing
  timeAsText w = timeText where
    t = time w
    minutes = floor (t/60)
    seconds = floor (t - 60 * (fromIntegral (floor (t/60))))
    rest =  floor . (100 * ) . snd . properFraction $ t
    timeText = (show minutes) ++ ":" ++ (show seconds) ++ ":" ++ (show rest)

  -- | Returns the current score of the world in a (player score):(AI score) format.
  scoreAsText :: World -- ^ the current world
    -> String -- ^ the current score
  scoreAsText world = score where
    s1 = show (p1Score world)
    s2 = show (p2Score world)
    score = s1 ++ ":" ++ s2
