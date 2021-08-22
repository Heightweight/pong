module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Text.Printf (printf)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Directory

data Result = Ongoing | Player | AI

data World = World {
game :: Game,
p1Score :: Int,
p2Score :: Int,
result :: Result,
time :: Float
}

data Game = Game {
p1 :: Float,
p2 :: Float,
ball :: Point,
dir :: Float,
vel :: Float
}
-- (800, 800)

startWorld :: World
startWorld = (World (Game 400 400 (400, 400) 0.72 300) 0 0 Ongoing 0)

simulationRate :: Int
simulationRate = 60

playerDraw :: Int -> Float -> Picture
playerDraw 1 x = Polygon [(x-50, 780), (x+50, 780), (x+50, 800), (x-50, 800)]
playerDraw _ x = Polygon [(x-50, 20), (x+50, 20), (x+50, 0), (x-50, 0)]

ballDraw :: Point -> Picture
ballDraw (x, y) = Translate x y (Circle 10)

displayTime :: World -> Picture
displayTime w = translate (0) (370) . scale 0.2 0.2 . text . timeAsText $ w

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

victory :: World -> Picture
victory world = pictures [translate (-350) 0 . text $ "You win!", translate 0 (-420) . displayTime $ world, translate (-200) (-50) . scale 0.2 0.2 . text $ "your time:"]


defeat :: World -> Picture
defeat world = pictures [translate (-350) 0 . text $ "You lose!", translate 0 (-420) . displayTime $ world, translate (-200) (-50) . scale 0.2 0.2 . text $ "your time:"]

worldDraw :: World -> Picture
worldDraw world = case (result world) of
  Ongoing -> pictures [(rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g]), translate (-400) (-400) . text . scoreAsText $ world, displayTime world] where
    g = game world
  Player -> victory world
  otherwise -> defeat world

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

updateWorld :: Float -> World -> World
updateWorld seconds world@(World _ _ _ Ongoing _ ) = gameOver . scoreCheck . aiMove $ moveBall seconds world
updateWorld _ world = world

eventHandler :: Event -> World -> World
eventHandler (EventMotion (x,y)) world = world {game = g1} where
  g = (game world)
  g1 = g {p1 = max 50 . min 750 $ (y+400)}
eventHandler (EventKey (Char 'r') Up _ _) world = startWorld {game = (game world) {p1 = p1 $ game world}}
eventHandler (EventKey (Char 's') Down _ _) world = unsafeResultSave world
eventHandler _ world = world

unsafeResultSave :: World -> World
unsafeResultSave world = case (result world) of
  Player -> unsafePerformIO $ do
    currDir <- getCurrentDirectory
    let file = currDir ++ "/records"
    appendFile file (timeAsText world ++ " " ++ (scoreAsText world))
    return world
  otherwise -> w


type WorldIO = StateT World IO

main :: IO ()
main = play
  (InWindow "pong" (800, 800) (300, 300))
  white
  simulationRate
  startWorld
  worldDraw
  eventHandler
  updateWorld
