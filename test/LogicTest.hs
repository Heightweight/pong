import App.Logic
import App.Draw.Render (leaderboardSort)
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
  , testCase "time to text" $ ("1:0:0") @=? (timeAsText $ startWorld {time = 60})
  , testCase "score to text" $ ("0:0") @=? (scoreAsText $ startWorld)
  , testCase "leaderboard sorting" $ ((head . leaderboardSort $ ["1:0:1 7:0", "1:0:0 7:1", "0:59:0 6:0"]) @=? "1:0:0 7:1")
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

averageFloat :: Gen Float
averageFloat = choose (1/30, 1/60)

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

xCollisionWorld :: Gen World
xCollisionWorld = do
  ballX <- oneof [choose (0, 9), choose (791, 800)] :: Gen Float
  ballY <- choose (200, 600) :: Gen Float
  w <- arbitrary :: Gen World
  return $ w {game = (game w) {ball = (ballX, ballY), dir = pi/2}}

yCollisionWorld :: Gen World
yCollisionWorld = do
  ballY <- oneof [choose (0, 9), choose (791, 800)] :: Gen Float
  ballX <- choose (200, 600) :: Gen Float
  w <- arbitrary :: Gen World
  return $ w {game = (game w) {ball = (ballX, ballY), dir = pi}}

pPaddleCollisionWorld :: Gen World
pPaddleCollisionWorld = do
  w <- arbitrary
  ballY <- choose (770, 800) :: Gen Float
  let paddleX = (p1 . game $ w)
  ballX <- choose (paddleX-50, paddleX + 50) :: Gen Float
  let ballXN = max 0 $ min 800 ballX
  return $ w {game = (game w) {ball = (ballXN, ballY), dir = pi/2}}

aiPaddleCollisionWorld :: Gen World
aiPaddleCollisionWorld = do
  w <- arbitrary
  ballY <- choose (0, 30) :: Gen Float
  let paddleX = (p2 . game $ w)
  ballX <- choose (paddleX-50, paddleX + 50) :: Gen Float
  let ballXN = max 0 $ min 800 ballX
  return $ w {game = (game w) {ball = (ballXN, ballY), dir = -pi/2}}

prop_pPaddleCollision :: Float -> Property
prop_pPaddleCollision f =
  forAll pPaddleCollisionWorld $
  \w -> ((dir . game $ w) /= (dir . game . (collisionPlayerPaddle f) $ w))

prop_aiPaddleCollision :: Float -> Property
prop_aiPaddleCollision f =
  forAll aiPaddleCollisionWorld $
  \w -> ((dir . game $ w) /= (dir . game . (collisionAIPaddle f) $ w))

prop_xCollision :: Float -> Property
prop_xCollision f =
  forAll xCollisionWorld $
  \w -> ((dir . game . (collisionX f) $ w) + (dir . game $ w) == pi)

prop_yCollision :: Float -> Property
prop_yCollision f =
  forAll yCollisionWorld $
  \w -> ((dir . game . (collisionY f) $ w) + (dir . game $ w) == 0)

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

prop_popBall :: World -> Bool
prop_popBall w = (ballX >= 11) && (ballX <= 789) && (ballY >= 11) && (ballY <= 789) where
  ballX = fst . ball . game . popBall $ w
  ballY = snd . ball . game . popBall $ w

prop_tickTime :: Float -> World -> Bool
prop_tickTime f w = ((time . (tickTime f) $ w) ==(time w) + f)

quickTests = testGroup "QuickCheck tests"
  [
  testProperty "normalize test" prop_normalized
  , testProperty "normalization identity test" prop_normalizedID
  , testProperty "spaced ball movement" (forAll smallFloat prop_spacedMovement)
  , testProperty "scoring worlds" (forAll scoreWorld prop_scoreCheck)
  , testProperty "non-scoring worlds" (forAll spacedBallWorld prop_noScoreCheck)
  , testProperty "ai behaviour" (forAll spacedBallWorld prop_aiBehaviour)
  , testProperty "checking player's wins" (forAll wonGame prop_playerWin)
  , testProperty "checking player's losses" (forAll lostGame prop_playerLoss)
  , testProperty "horizontal collision" (forAll averageFloat prop_xCollision)
  , testProperty "vertical collision" (forAll averageFloat prop_yCollision)
  , testProperty "player paddle collision" (forAll averageFloat prop_pPaddleCollision)
  , testProperty "ai paddle collision" (forAll averageFloat prop_aiPaddleCollision)
  , testProperty "checking collision resolvation" prop_popBall
  , testProperty "time tick" (forAll smallFloat prop_tickTime)
  ]
