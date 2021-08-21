module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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

gameDraw :: Game -> Picture
gameDraw g = translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g, text (show (dir g))]

paddleAngle :: Float -> Float -> Float
paddleAngle paddle ball = (pi/8)*(1 + (ball - paddle)/50)/2 + 7*(pi/8)*(1 -(1 + (ball - paddle)/50)/2)

normalize :: Float -> Float
normalize f = max 11 . min 789 $ f

moveBall :: Game -> Float -> Game
moveBall g f = g {dir = dirn, ball = (xt, yt)}
 where
  xn = fst (ball g) + vel g * cos (dir g)
  yn = snd (ball g) + vel g * sin (dir g)
  dirx
    |(max 10 $ min 790 xn) == xn = 1
    |otherwise = -1
  diry
    |(max 10 $ min 790 yn) == yn = 1
    |otherwise = -1
  dirn
    |dirx == -1 = pi - diry * dir g
    |(diry == -1) && (dirx == 1) = diry * dir g
    |max (p1 g - 50) (min (p1 g + 50) xn) == xn && (yn <= 30) = paddleAngle (p1 g) xn
    |max (p2 g - 50) (min (p2 g + 50) xn) == xn && (yn >= 770) = - paddleAngle (p2 g) xn
    |otherwise = (dir g)
  xt = (vel g * cos dirn) + normalize (fst (ball g))
  yt = (vel g * sin dirn) + normalize (snd (ball g))

movePaddle :: Event -> Game -> Game
movePaddle (EventMotion (x,y)) g = g {nextPos = max 50 . min 750 $ (x+400)}
movePaddle _ g = g

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
  (Game 400 400 (400, 400) 0.72 5 400)
  gameDraw
  movePaddle
  updatePaddle . (flip moveBall)
