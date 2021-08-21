module Main where

import Lib
import Graphics.Gloss

data Game = Game {
p1 :: Float,
p2 :: Float,
ball :: Point,
dir :: Float,
vel :: Float
}
-- (800, 800)

playerDraw :: Int -> Float -> Picture
playerDraw 1 x = Polygon [(x-50, 780), (x+50, 780), (x+50, 800), (x-50, 800)]
playerDraw _ x = Polygon [(x-50, 20), (x+50, 20), (x+50, 0), (x-50, 0)]

ballDraw :: Point -> Picture
ballDraw (x, y) = Translate x y (Circle 10)

gameDraw :: Game -> Picture
gameDraw g = translate (-400) (-400) $ pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g, text (show (dir g))]

moveBall :: Game -> Float -> Game
moveBall g f = Game (p1 g) (p2 g) (xt, yt) dirn (vel g)
 where
  x1n = max 11 $ min 789 (fst $ ball g)
  y1n = max 11 $ min 789 (snd $ ball g)
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
    |otherwise = diry * dir g
  xt = x1n + vel g * cos dirn
  yt = y1n + vel g * sin dirn


next _ f g = moveBall g f

main :: IO ()
main = simulate
  (InWindow "pong" (800, 800) (300, 300))
  white
  60
  (Game 400 400 (400, 400) 0.7 2)
  gameDraw
  next
