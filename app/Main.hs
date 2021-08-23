module Main where

import App.Engine
import App.Logic
import App.Draw.Render
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
