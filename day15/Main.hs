import Text.Parsec.String (GenParser)
import Text.Parsec (many, many1, (<|>), parse, sepBy)
import Text.Parsec.Char (endOfLine, char, string, digit)
import qualified Data.Set as Set (Set, insert, empty, size)


testFileName = "test.txt"
testy = 10
testmax = 20
inputFileName = "input.txt"
inputy = 2000000
inputmax = 4000000

input :: GenParser Char st [((Int, Int), (Int, Int))]
input = many sbPair where
  sbPair = do
    string "Sensor at x="
    sx <- number
    string ", y="
    sy <- number
    string ": closest beacon is at x="
    bx <- number
    string ", y="
    by <- number
    endOfLine
    return ((sx, sy), (bx, by))
  number = negNumber <|> posNumber
  negNumber = do
    char '-'
    n <- many digit
    return (- read n)
  posNumber = do
    n <- many digit
    return $ read n

-- insert a new interval into a collection of pairwise disjoint intervals that
-- do not touch each other so that these properties are preserved
insertInt :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertInt i [] = [i]
insertInt (newa, newb) ((a, b):is)
  | newb < a-1 || b < newa-1 = (a, b):insertInt (newa, newb) is -- no insersection
  | otherwise = insertInt (min a newa, max b newb) is

updateInfo :: Int -> ([(Int, Int)], Set.Set Int) -> ((Int, Int), (Int, Int)) -> ([(Int, Int)], Set.Set Int)
updateInfo y (ints, beacons) ((sx, sy), (bx, by)) = (newints, newbeacons) where
  dy = abs(y - sy)                       -- vertical distance between sensor and row
  distsb = abs(sx - bx) + abs (sy - by)  -- distance between sensor and beacer in l1-metric
  newints = if dy > distsb then ints else insertInt (sx - distsb + dy, sx + distsb - dy) ints
  newbeacons = if by == y then Set.insert bx beacons else beacons

part1 :: Int -> [((Int, Int), (Int, Int))] -> Int
part1 y ms = n where
  n = sumints - sumbeacons
  sumints = sum $ map (\(a, b) -> b - a + 1) ints
  sumbeacons = Set.size beacons
  (ints, beacons) = foldl (updateInfo y) ([], Set.empty) ms

tuningFrequency :: Int -> Int -> Int
tuningFrequency x y = 4000000 * x + y

uncovered :: [(Int, Int)] -> (Int, Int) -> Maybe Int
uncovered [] (c, d) = Just c -- I don't think this should ever happen
uncovered ((a, b):is) (c, d)
  | a <= c && d <= b = Nothing
  | a <= c && b < d = Just $ b + 1
  | otherwise = uncovered is (c, d)

part2 :: Int -> Int -> [((Int, Int), (Int, Int))] -> Int
part2 maxx maxy ms = n where
  n = case uncovered ints (0, maxx) of
    Nothing -> part2 maxx (maxy - 1) ms
    Just x -> tuningFrequency x maxy
  (ints, _) = foldl (updateInfo maxy) ([], Set.empty) ms

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 testy <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 inputy <$> parse input "Error" mainInput)
  putStrLn $ "Part 2 Test: " ++ show (part2 testmax testmax <$> parse input "Error" testInput)
  putStrLn $ "Part 2 Main: " ++ show (part2 inputmax inputmax <$> parse input "Error" mainInput)
