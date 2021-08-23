module App.Engine where

  import Graphics.Gloss.Data.Point.Arithmetic

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

  data Result = Ongoing | Player | AI | Idle | Pause deriving (Eq)

  simulationRate :: Int
  simulationRate = 60

  startWorld :: World
  startWorld = (World (Game 400 400 (400, 400) 0.72 300) 0 0 Idle 0 0)
