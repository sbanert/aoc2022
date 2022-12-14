import Text.Parsec.String (GenParser)
import Text.Parsec (many, parse, sepBy)
import Text.Parsec.Char (endOfLine, char, string, digit)
import qualified Data.Set as Set (Set, insert, empty, fromList, findMin, findMax, member, notMember, map, union, fromList)

testFileName = "test.txt"
inputFileName = "input.txt"

input :: GenParser Char st [[(Int, Int)]]
input = many path where
  path = do
    cs <- coords `sepBy` string " -> "
    endOfLine
    return cs
  coords = do
    x <- many digit
    char ','
    y <- many digit
    return (read x, read y)

rockCoords :: [[(Int, Int)]] -> Set.Set (Int, Int)
rockCoords [] = Set.empty
rockCoords ([]:ps) = rockCoords ps
rockCoords ([p]:ps) = Set.insert p $ rockCoords ps
rockCoords (((x1, y1):(x2, y2):cs):ps)
  | x1 == x2 && y1 == y2 = rockCoords (((x2, y2):cs):ps)
  | x1 == x2 && y1 > y2 = Set.insert (x1, y1) $ rockCoords (((x1, y1 - 1):(x2, y2):cs):ps)
  | x1 == x2 && y1 < y2 = Set.insert (x1, y1) $ rockCoords (((x1, y1 + 1):(x2, y2):cs):ps)
  | x1 > x2 && y1 == y2 = Set.insert (x1, y1) $ rockCoords (((x1 - 1, y1):(x2, y2):cs):ps)
  | x1 < x2 && y1 == y2 = Set.insert (x1, y1) $ rockCoords (((x1 + 1, y2):(x2, y2):cs):ps)
  | otherwise = undefined

insertSand :: (Int, Int) -> Set.Set (Int, Int) -> (Maybe (Int, Int), Set.Set (Int, Int))
insertSand (x, y) rs
  | (x, y) `Set.member` rs = (Nothing, rs)
  | y > Set.findMax (Set.map snd rs) = (Nothing, rs)
  | (x, y+1) `Set.notMember` rs = insertSand (x, y+1) rs
  | (x-1, y+1) `Set.notMember` rs = insertSand (x-1, y+1) rs
  | (x+1, y+1) `Set.notMember` rs = insertSand (x+1, y+1) rs
  | otherwise = (Just (x, y), Set.insert (x, y) rs)

sandSource :: (Int, Int)
sandSource = (500, 0)

sandCapacity :: Set.Set (Int, Int) -> Int
sandCapacity rs = case insertSand sandSource rs of
  (Nothing, _) -> 0
  (Just _, rs) -> 1 + sandCapacity rs

part1 :: [[(Int, Int)]] -> Int
part1 = sandCapacity . rockCoords

addGround :: Set.Set (Int, Int) -> Set.Set (Int, Int)
addGround rs = Set.union rs (Set.fromList [(x, yg) | x <- [xmin..xmax]])
  where yg = Set.findMax (Set.map snd rs) + 2
        xmin = Set.findMin (Set.map fst rs) - yg + 2
        xmax = Set.findMax (Set.map fst rs) + yg + 2

part2 = sandCapacity . addGround . rockCoords

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 <$> parse input "Error" mainInput)
  putStrLn $ "Part 2 Test: " ++ show (part2 <$> parse input "Error" testInput)
  putStrLn $ "Part 2 Main: " ++ show (part2 <$> parse input "Error" mainInput)
