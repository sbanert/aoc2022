import Data.List.Split (splitOn)

testFileName = "test.txt"
inputFileName = "input.txt"

main :: IO ()
main = do
  testInput <- getInputFromFile testFileName
  mainInput <- getInputFromFile inputFileName
  putStrLn $ "Part 1 Test: " ++ (show $ countContainingPairs testInput)
  putStrLn $ "Part 1 Real: " ++ (show $ countContainingPairs mainInput)
  putStrLn $ "Part 2 Test: " ++ (show $ countOverlappingPairs testInput)
  putStrLn $ "Part 2 Real: " ++ (show $ countOverlappingPairs mainInput)

getInputFromFile :: String -> IO [String]
getInputFromFile fileName = do
  contents <- readFile fileName
  return $ lines contents

-- Thre is a lot that can go wrong here if the input is not as expected.
getIntervals :: String -> ((Int, Int), (Int, Int))
getIntervals xs = ((read a1, read a2), (read b1, read b2))
  where a:b:_ = splitOn "," xs
        a1:a2:_ = splitOn "-" a
        b1:b2:_ = splitOn "-" b

contains :: (Int, Int) -> (Int, Int) -> Bool
contains (a1, a2) (b1, b2) = b1 >= a1 && b2 <= a2

oneFullyContainsOther :: ((Int, Int), (Int, Int)) -> Bool
oneFullyContainsOther (a, b) = a `contains` b || b `contains` a

count :: (a -> Bool) -> [a] -> Int
count p [] = 0
count p (x:xs) = if p x then 1 + count p xs else count p xs

countContainingPairs :: [String] -> Int
countContainingPairs = count id . map (oneFullyContainsOther . getIntervals)

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a1, a2), (b1, b2)) = max a1 b1 <= min a2 b2

countOverlappingPairs :: [String] -> Int
countOverlappingPairs = count id . map (overlap . getIntervals)
