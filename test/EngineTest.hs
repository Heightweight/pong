import App.Engine
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain (testGroup "Engine Test" [testSimulationRate])

testSimulationRate :: TestTree
testSimulationRate = testCase "Checking simulation rate..."
  (assertEqual "Should be 60" 60 simulationRate)
