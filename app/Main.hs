module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


data World = World {
game :: Game,
p1Score :: Int,
p2Score :: Int
}

data Game = Game {
p1 :: Float,
p2 :: Float,
ball :: Point,
dir :: Float,
vel :: Float,
nextPos :: Float
}
-- (800, 800)

playerDraw :: Int -> Float -> Picture
playerDraw 1 x = Polygon [(x-50, 780), (x+50, 780), (x+50, 800), (x-50, 800)]
playerDraw _ x = Polygon [(x-50, 20), (x+50, 20), (x+50, 0), (x-50, 0)]

ballDraw :: Point -> Picture
ballDraw (x, y) = Translate x y (Circle 10)

worldDraw :: World -> Picture
worldDraw world = pictures [(rotate (-90) $ translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g]), translate (-400) (-400) $ text score] where
  g = game world
  s1 = show (p1Score world)
  s2 = show (p2Score world)
  score = s1 ++ ":" ++ s2

paddleAngle :: Float -> Float -> Float
paddleAngle paddle ball = (pi/8)*(1 + (ball - paddle)/50)/2 + 7*(pi/8)*(1 -(1 + (ball - paddle)/50)/2)

normalize :: Float -> Float
normalize f = max 11 . min 789 $ f

moveBall :: Float -> World -> World
moveBall f w = w {game = (game w) {dir = dirn, ball = (xt, yt)}}
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

eventHandler :: Event -> World -> World
eventHandler (EventMotion (x,y)) world = world {game = g1} where
  g = (game world)
  g1 = g {p1 = max 50 . min 750 $ (y+400)}
eventHandler (EventKey (Char 'r') Up _ _) world = (World (Game (p1 $ game world) 400 (400, 400) 0.72 300 400) 0 0)
eventHandler _ world = world

updatePaddle :: Game -> Game
updatePaddle g = g {p1 = nextPos g}

eventTracker :: IO ()
eventTracker
 = play (InWindow "GameEvent" (700, 100) (10, 10))
        white
        100
        ""
        (\str     -> Translate (-340) 0 $ Scale 0.1 0.1 $ Text str)
        (\event _ -> show event)
        (\_ world -> world)

main :: IO ()
main = play
  (InWindow "pong" (800, 800) (300, 300))
  white
  60
  (World (Game 400 400 (400, 400) 0.72 300 400) 0 0)
  worldDraw
  eventHandler
  moveBall
