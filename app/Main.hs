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
gameDraw g = Pictures [playerDraw 1 $ p1 g, playerDraw 2 $ p2 g, ballDraw $ ball g]

moveBall :: Game -> Float -> Game
moveBall g f = Game (p1 g) (p2 g) (xt, yt) dirn (vel g)
 where
  x1n = min 0 $ max 800 (fst $ ball g)
  y1n = min 0 $ max 800 (snd $ ball g)
  xn = (fst $ ball g) + (vel g) * (cos $ dir g)
  yn = (snd $ ball g) + (vel g) * (sin $ dir g)
  dirx
    |(min 0 $ max 800 xn) == xn = 1
    |otherwise = -1
  diry
    |(min 0 $ max 800 yn) == yn = 1
    |otherwise = -1
  xt = x1n + dirx * (vel g) * (cos $ dir g)
  yt = y1n + diry * (vel g) * (sin $ dir g)
  dirn = 180 + diry * (360 + dirx * (dir g))

next _ d g = undefined

main :: IO ()
main = animate FullScreen white (\f -> (Circle f))
