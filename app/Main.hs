module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Trans
import System.Directory
import Data.List.Split
import Data.List

data Result = Ongoing | Player | AI | Idle

data World = World {
game :: Game,
p1Score :: Int,
p2Score :: Int,
result :: Result,
time :: Float,
idleTime :: Float
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
startWorld = (World (Game 400 400 (400, 400) 0.72 300) 0 0 Idle 0 0)

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

type Record = String

recordUncurry :: Record -> (Float, Int)
recordUncurry r = (time, score) where
  records = splitOn " " r
  undone = map (splitOn ":") records
  time :: Float
  score :: Int
  time = 60*(read ((head undone) !! 0)) + read ((head undone) !! 1) + (read ((head undone) !! 2))/100
  score = read . last . last $ undone

leaderboardSort :: [Record] -> [Record]
leaderboardSort = sortOn (fst . recordUncurry)

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
--  Ongoing -> pictures [(rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g]), translate (-400) (-400) . text . scoreAsText $ world, displayTime world] where
--    g = game world
--  Player -> victory world
--  otherwise -> defeat world

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
eventHandler (EventKey (Char 's') Down _ _) world = case (result world) of
  Player -> do
    currDir <- getCurrentDirectory
    let file = currDir ++ "/records"
    appendFile file (timeAsText world ++ " " ++ (scoreAsText world) ++ "\n")
    return world
  otherwise -> do
    return world
eventHandler _ world = do
  return world

main :: IO ()
main = playIO
  (InWindow "pong!" (800, 800) (0, 0))
  black
  simulationRate
  startWorld
  worldDraw
  eventHandler
  updateWorld
