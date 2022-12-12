import Data.Char (ord)
import Data.List (transpose)

testFileName = "test.txt"
inputFileName = "input.txt"

initialise :: Char -> Maybe Int
initialise 'E' = Just 0
initialise _ = Nothing

height :: Char -> Int
height 'E' = ord 'z'
height 'S' = ord 'a'
height c = ord c

improveLine :: [Int] -> [Maybe Int] -> [Maybe Int]
improveLine _ [] = []
improveLine _ [a] = [a]
improveLine (h1:h2:hs) (a1:a2:as) = case (a1, a2) of
  (Nothing, Nothing) -> Nothing:improveLine (h2:hs) (a2:as)
  (Just a, Just b)   -> Just a:improveLine (h2:hs) (a2:as)
  (Just a, Nothing)  -> Just a:improveLine (h2:hs) ((if h2 >= h1 - 1 then Just (a + 1) else Nothing):as)
  (Nothing, Just b)  -> (if h1 >= h2 - 1 then Just (b + 1) else Nothing):improveLine (h2:hs) (a2:as)

improveH :: [[Int]] -> [[Maybe Int]] -> [[Maybe Int]]
improveH = zipWith improveLine

improveV :: [[Int]] -> [[Maybe Int]] -> [[Maybe Int]]
improveV heights oldmap = transpose $ improveH (transpose heights) (transpose oldmap)

improve :: [[Int]] -> [[Maybe Int]] -> [[Maybe Int]]
improve heights oldmap = zipWith (zipWith mapHelper) (improveH heights oldmap) (improveV heights oldmap)

mapHelper :: Maybe Int -> Maybe Int -> Maybe Int
mapHelper Nothing Nothing = Nothing
mapHelper (Just a) Nothing = Just a
mapHelper Nothing (Just a) = Just a
mapHelper (Just a) (Just b) = Just (min a b)

getStart :: [Char] -> [Maybe Int] -> Maybe Int
getStart (i:is) (m:ms) = if i == 'S' then m else getStart is ms

getClosestStart :: [Char] -> [Maybe Int] -> Maybe Int
getClosestStart [] [] = Nothing
getClosestStart (i:is) (m:ms) = if i == 'S' || i == 'a' then mapHelper m (getClosestStart is ms) else getClosestStart is ms

part1 :: [[Char]] -> Int
part1 input = part1Helper ii where
  hi = map (map height) input
  ii = map (map initialise) input
  part1Helper m = case getStart (concat input) (concat m) of
    Nothing -> part1Helper (improve hi m)
    Just a -> a

part2 :: [[Char]] -> Int
part2 input = part2Helper ii where
  hi = map (map height) input
  ii = map (map initialise) input
  part2Helper m = case getClosestStart (concat input) (concat m) of
    Nothing -> part2Helper (improve hi m)
    Just a -> a

main :: IO ()
main = do
  testInput <- lines <$> readFile testFileName
  mainInput <- lines <$> readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 mainInput)
  putStrLn $ "Part 2 Test: " ++ show (part2 testInput)
  putStrLn $ "Part 2 Main: " ++ show (part2 mainInput)
