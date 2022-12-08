import Data.Char (ord)
import Data.List (transpose)

testFileName = "test.txt"
inputFileName = "input.txt"

heights :: String -> [[Int]]
heights input = map (map (\ c -> ord c - ord '0')) $ lines input

visible :: [Int] -> [Bool]
visible = visibleHelper (-1)
  where visibleHelper _ [] = []
        visibleHelper h (d:ds) = if d > h then True:visibleHelper d ds else False:visibleHelper h ds

scenic :: [Int] -> [Int]
scenic = scenicHelper []
  where
    scenicHelper :: [Int] -> [Int] -> [Int]
    scenicHelper _ [] = []
    scenicHelper v (d:ds) = min (length v) (1 + length (takeWhile (< d) v)) : scenicHelper (d:v) ds

visibleFromLeft = map visible
visibleFromRight = map (reverse . visible . reverse)
visibleFromTop = transpose . visibleFromLeft . transpose
visibleFromBottom = transpose . visibleFromRight . transpose

scenicLeft = map scenic
scenicRight = map (reverse . scenic . reverse)
scenicTop = transpose . scenicLeft . transpose
scenicBottom = transpose . scenicRight . transpose

treeMapOr :: [[Bool]] -> [[Bool]] -> [[Bool]]
treeMapOr = zipWith (zipWith (||))

-- I guess I could just flatten these. Oh well ...
treeMapTimes :: [[Int]] -> [[Int]] -> [[Int]]
treeMapTimes = zipWith (zipWith (*))

visibleTrees :: [[Int]] -> [[Bool]]
visibleTrees m = visibleFromLeft m `treeMapOr` visibleFromRight m `treeMapOr` visibleFromTop m `treeMapOr` visibleFromBottom m

scenicScore :: [[Int]] -> [[Int]]
scenicScore m = scenicLeft m `treeMapTimes` scenicRight m `treeMapTimes` scenicTop m `treeMapTimes` scenicBottom m

treeMapMax :: [[Int]] -> Int
treeMapMax m = maximum (map maximum m)

treeMapCount :: (a -> Bool) -> [[a]] -> Int
treeMapCount p = sum . map (length . filter p)

part1 = treeMapCount id . visibleTrees . heights
part2 = treeMapMax . scenicScore . heights

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 mainInput)
  putStrLn $ "Part 2 Test: " ++ show (part2 testInput)
  putStrLn $ "Part 2 Main: " ++ show (part2 mainInput)
