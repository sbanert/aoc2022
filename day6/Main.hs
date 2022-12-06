import Data.Set (fromList, size)

testFileName = "test.txt"
inputFileName = "input.txt"

allDifferent :: [Char] -> Bool
allDifferent xs = size (fromList xs) == length xs

firstMarker :: Int -> [Char] -> Int
firstMarker n xs
  | allDifferent (take n xs) = n
  | otherwise = 1 + firstMarker n (drop 1 xs)

main :: IO ()
main = do
  testInput <- readFile "test.txt"
  mainInput <- readFile "input.txt"
  putStrLn $ "Part 1 Test: " ++ show (firstMarker 4 testInput)
  putStrLn $ "Part 1 Main: " ++ show (firstMarker 4 mainInput)
  putStrLn $ "Part 2 Test: " ++ show (firstMarker 14 testInput)
  putStrLn $ "Part 2 Main: " ++ show (firstMarker 14 mainInput)
