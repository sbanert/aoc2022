import System.IO
import Data.List
import Data.List.Split

testFileName = "test.txt"
inputFileName = "input.txt"

data HandShape = Rock | Paper | Scissors
data Outcome   = Win  | Loss  | Tie

readHand :: String -> HandShape
readHand "A" = Rock
readHand "B" = Paper
readHand "C" = Scissors
readHand "X" = Rock
readHand "Y" = Paper
readHand "Z" = Scissors
readHand _   = Rock -- should never happen

readOutcome :: String -> Outcome
readOutcome "X" = Loss
readOutcome "Y" = Tie
readOutcome "Z" = Win
readOutcome _   = Loss -- should never happen

main = do
  testHandShapes <- getHandShapesFromFile testFileName
  mainHandShapes <- getHandShapesFromFile inputFileName
  testStrategies <- getStrategiesFromFile testFileName
  mainStrategies <- getStrategiesFromFile inputFileName
  putStrLn $ "Part 1 Test: " ++ (show $ sum $ map totalPoints testHandShapes)
  putStrLn $ "Part 1 Real: " ++ (show $ sum $ map totalPoints mainHandShapes)
  putStrLn $ "Part 2 Test: " ++ (show $ sum $ map (totalPoints . strToHs) testStrategies)
  putStrLn $ "Part 2 Real: " ++ (show $ sum $ map (totalPoints . strToHs) mainStrategies)

getHandShapesFromFile :: String -> IO [(HandShape, HandShape)]
getHandShapesFromFile fileName = do
  contents <- readFile fileName
  return $ getHandShapesFromStrings $ lines contents

getHandShapesFromStrings :: [String] -> [(HandShape, HandShape)]
getHandShapesFromStrings contents = map (\(op:me:_) -> (readHand op, readHand me)) $ map (splitOn " ") contents

getStrategiesFromFile :: String -> IO [(HandShape, Outcome)]
getStrategiesFromFile fileName = do
  contents <- readFile fileName
  return $ getStrategiesFromStrings $ lines contents
  
getStrategiesFromStrings :: [String] -> [(HandShape, Outcome)]
getStrategiesFromStrings contents = map (\(op:oc:_) -> (readHand op, readOutcome oc)) $ map (splitOn " ") contents

totalPoints :: (HandShape, HandShape) -> Int
totalPoints (op, me) = shapePoints me + outcome me op
  where
    shapePoints Rock = 1
    shapePoints Paper = 2
    shapePoints Scissors = 3
    outcome Rock Scissors = 6
    outcome Scissors Paper = 6
    outcome Paper Rock = 6
    outcome Rock Rock = 3
    outcome Paper Paper = 3
    outcome Scissors Scissors = 3
    outcome _ _ = 0
  
strToHs :: (HandShape, Outcome) -> (HandShape, HandShape)
strToHs (a, Tie) = (a, a)
strToHs (Rock, Loss) = (Rock, Scissors)
strToHs (Rock, Win) = (Rock, Paper)
strToHs (Paper, Loss) = (Paper, Rock)
strToHs (Paper, Win) = (Paper, Scissors)
strToHs (Scissors, Loss) = (Scissors, Paper)
strToHs (Scissors, Win) = (Scissors, Rock)
