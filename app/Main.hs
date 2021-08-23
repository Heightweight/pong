module Main where

import Lib
import Engine
import Logic
import Render
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO
  (InWindow "pong!" (800, 800) (0, 0))
  black
  simulationRate
  startWorld
  worldDraw
  eventHandler
  updateWorld
