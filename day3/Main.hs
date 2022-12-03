import System.IO
import Data.List (intersect)
import Data.Char (ord)
import Control.Monad ((<=<), liftM2)

testFileName = "test.txt"
inputFileName = "input.txt"

main :: IO ()
main = do
  testContents <- getContentsFromFile testFileName
  mainContents <- getContentsFromFile inputFileName
  putStrLn $ "Part 1 Test: " ++ (show $ sumDuplicatePriorities testContents)
  putStrLn $ "Part 1 Real: " ++ (show $ sumDuplicatePriorities mainContents)
  putStrLn $ "Part 2 Test: " ++ (show $ sumBadgePriorities testContents)
  putStrLn $ "Part 2 Real: " ++ (show $ sumBadgePriorities mainContents)

getContentsFromFile :: String -> IO [[Char]]
getContentsFromFile fileName = do
  contents <- readFile fileName
  return $ lines contents

getCompartments :: [Char] -> ([Char], [Char])
getCompartments xs = splitAt l xs
  where l = length xs `div` 2

getDuplicate :: ([Char], [Char]) -> Maybe Char
getDuplicate (xs, ys) = case xs `intersect` ys of
  []    -> Nothing -- should never happen if input is correctly formatted
  (x:_) -> Just x

getPriority :: Char -> Maybe Int
getPriority c
  | 'a' <= c && c <= 'z' = Just (ord c - ord 'a' + 1)
  | 'A' <= c && c <= 'Z' = Just (ord c - ord 'A' + 27)
  | otherwise = Nothing -- should never happen if only letters

sumDuplicatePriorities :: [[Char]] -> Maybe Int
sumDuplicatePriorities = fmap sum . mapM (getPriority <=< getDuplicate . getCompartments)

getGroups :: [[Char]] -> Maybe [([Char], [Char], [Char])]
getGroups [] = Just []
getGroups (a:b:c:cs) = liftM2 (:) (Just (a, b, c)) (getGroups cs)
getGroups _ = Nothing -- should never happen if number of lines divisible by 3

getBadge :: ([Char], [Char], [Char]) -> Maybe Char
getBadge (a, b, c) = case a `intersect` b `intersect` c of
  [] -> Nothing -- should never happen if there is a badge
  (x:_) -> Just x

sumBadgePriorities :: [[Char]] -> Maybe Int
sumBadgePriorities = fmap sum . (mapM (getPriority <=< getBadge) <=< getGroups)
