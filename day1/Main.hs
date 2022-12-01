import System.IO
import Data.List
import Data.List.Split

testFileName = "test.txt"
inputFileName = "input.txt"

main = do
  testCalories <- getCaloriesFromFile testFileName
  mainCalories <- getCaloriesFromFile inputFileName
  print testCalories
  putStrLn ("Part 1 Test: " ++ (show $ biggestElf testCalories))
  putStrLn ("Part 1 Real: " ++ (show $ biggestElf mainCalories))
  putStrLn ("Part 2 Test: " ++ (show $ topThreeElves testCalories))
  putStrLn ("Part 2 Real: " ++ (show $ topThreeElves mainCalories))

getCaloriesFromFile :: String -> IO [[Int]]
getCaloriesFromFile fileName = do
  contents <- readFile fileName
  return $ getCaloriesFromStrings $ lines contents

getCaloriesFromStrings :: [String] -> [[Int]]
getCaloriesFromStrings contents = map (map read) $ splitOn [""] contents

biggestElf :: [[Int]] -> Int
biggestElf calories = maximum (map sum calories)

topThreeElves :: [[Int]] -> Int
topThreeElves calories = sum $ take 3 $ sortBy (flip compare) (map sum calories)





  


  
