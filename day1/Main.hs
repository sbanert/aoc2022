import System.IO
import Data.List
import Data.List.Split

testFileName = "test.txt"
inputFileName = "input.txt"

main = do
  testCalories <- getCaloriesFromFile testFileName
  mainCalories <- getCaloriesFromFile inputFileName
  putStrLn $ "Part 1 Test: " ++ (show $ topElves 1 testCalories)
  putStrLn $ "Part 1 Real: " ++ (show $ topElves 1 mainCalories)
  putStrLn $ "Part 2 Test: " ++ (show $ topElves 3 testCalories)
  putStrLn $ "Part 2 Real: " ++ (show $ topElves 3 mainCalories)

getCaloriesFromFile :: String -> IO [[Int]]
getCaloriesFromFile fileName = do
  contents <- readFile fileName
  return $ getCaloriesFromStrings $ lines contents

getCaloriesFromStrings :: [String] -> [[Int]]
getCaloriesFromStrings contents = map (map read) $ splitOn [""] contents

topElves :: Int -> [[Int]] -> Int
topElves n calories = sum $ take n $ sortBy (flip compare) (map sum calories)





  


  
