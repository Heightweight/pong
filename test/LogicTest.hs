import App.Logic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck hiding(Result)

main :: IO ()
main = do
  defaultMain (testGroup "Logic tests" [unitTests, quickTests])

--startWorld

unitTests = testGroup "Unit tests"
  [ testCase "player paddle" $ 400 @=? (p1 $ game startWorld)
  , testCase "recordUncurry time" $ 48.21 @=? (fst . recordUncurry $ "0:48:21 7:0")
  , testCase "recordUncurry score" $ 7 @=? (snd . recordUncurry $ "0:48:21 7:0")
  , testCase "paddle angle right" $ (pi/8) @=? (paddleAngle 230 280)
  , testCase "paddle angle left" $ (7*pi/8) @=? (paddleAngle 50 0)
  , testCase "paddle angle middle" $ (pi/2) @=? (paddleAngle 599 599)
  ]

instance Arbitrary Game where
  arbitrary = do
    paddle1 <- choose (50, 750) :: Gen Float
    paddle2 <- choose (50, 750) :: Gen Float
    ballX <- choose (0, 800) :: Gen Float
    ballY <- choose (0, 800) :: Gen Float
    dirN <- choose (0, 2*pi) :: Gen Float
    let velN = 300
    return $ (Game paddle1 paddle2 (ballX, ballY) dirN velN)

instance Arbitrary Result where
  arbitrary = do
    n <- chooseInt (0, 4)
    return $ case n of
      0 -> Ongoing
      1 -> Player
      2 -> AI
      3 -> Idle
      4 -> Paused

instance Arbitrary World where
  arbitrary = do
    g <- arbitrary :: Gen Game
    p1S <- chooseInt (0, 7)
    p2S <- chooseInt (0, 7)
    res <- arbitrary :: Gen Result
    t <- choose (0, 60) :: Gen Float
    iT <- choose (0, 60) :: Gen Float
    return $ (World g p1S p2S res t iT)

prop_normalized f = (normalize f) <= 789 && (normalize f) >= 11
  where types = f::Float

prop_normalizedID f = (normalize . normalize $ f) == (normalize f)
  where types = f::Float

smallFloat :: Gen Float
smallFloat = choose (0, 1/60)

spacedBallWorld :: Gen World
spacedBallWorld = do
  ballX <- choose (200, 600) :: Gen Float
  ballY <- choose (200, 600) :: Gen Float
  w <- arbitrary :: Gen World
  return $ w {game = (game w) {ball = (ballX, ballY)}, result = Ongoing, p1Score = 0, p2Score = 0}

wonGame :: Gen World
wonGame = do
  w <- arbitrary :: Gen World
  p2S <- chooseInt (0, 6)
  return $ w {p1Score = 7, p2Score = p2S}

lostGame :: Gen World
lostGame = do
  w <- arbitrary :: Gen World
  p1S <- chooseInt (0, 6)
  return $ w {p1Score = p1S, p2Score = 7}

scoreWorld :: Gen World
scoreWorld = do
  ballY <- oneof [choose (0, 15), choose (785, 800)] :: Gen Float
  ballX <- choose (200, 600) :: Gen Float
  w <- arbitrary :: Gen World
  return $ w {game = (game w) {ball = (ballX, ballY)}}


prop_spacedMovement :: Float -> Property
prop_spacedMovement f =
  forAll spacedBallWorld $
  \w -> (((fst . ball . game $ w) - (fst . ball . game . moveBall f $ w))^2 + ((snd . ball . game $ w) - (snd . ball . game . moveBall f $ w))^2 - (f*300)^2) < 0.01

prop_scoreCheck :: World -> Bool
prop_scoreCheck w = (p1Score w /= (p1Score . scoreCheck $ w)) || (p2Score w /= (p2Score . scoreCheck $ w))

prop_noScoreCheck :: World -> Bool
prop_noScoreCheck = not . prop_scoreCheck

prop_aiBehaviour :: World -> Bool
prop_aiBehaviour w = (abs newDist <= abs oldDist) where
  wNext = aiMove w
  old = p2 . game $ w
  new = p2 . game $ wNext
  oldDist = old - fst (ball (game w))
  newDist = new - fst (ball (game w))

prop_playerWin :: World -> Bool
prop_playerWin w = (Player ==) . result . gameOver $ w

prop_playerLoss :: World -> Bool
prop_playerLoss w = (AI ==) . result . gameOver $ w


quickTests = testGroup "QuickCheck tests"
  [
  testProperty "normalization test" prop_normalized
  , testProperty "normalization identity test" prop_normalizedID
  , testProperty "spaced ball movement" (forAll smallFloat prop_spacedMovement)
  , testProperty "scoring worlds" (forAll scoreWorld prop_scoreCheck)
  , testProperty "non-scoring worlds" (forAll spacedBallWorld prop_noScoreCheck)
  , testProperty "ai behaviour" (forAll spacedBallWorld prop_aiBehaviour)
  , testProperty "checking player's wins" (forAll wonGame prop_playerWin)
  , testProperty "checking player's losses" (forAll lostGame prop_playerLoss)
  ]
