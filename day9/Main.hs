import Text.Parsec.String (GenParser)
import Text.Parsec (many, (<|>), parse)
import Text.Parsec.Char (endOfLine, char, digit)
import Data.Set (fromList, size)

testFileName = "test.txt"
test2FileName = "test2.txt"
inputFileName = "input.txt"

data Direction = L | R | U | D

input :: GenParser Char st [(Int, Direction)]
input = many line
  where line = do
          d <- dirL <|> dirR <|> dirU <|> dirD
          char ' '
          n <- many digit
          endOfLine
          return (read n, d)
        dirL = char 'L' >> return L
        dirR = char 'R' >> return R
        dirU = char 'U' >> return U
        dirD = char 'D' >> return D

writeOut :: [(Int, Direction)] -> [Direction]
writeOut [] = []
writeOut ((0, _):r) = writeOut r
writeOut ((n, d):r) = d:writeOut((n-1, d):r)

move :: Direction -> (Int, Int) -> (Int, Int)
move L (x, y) = (x-1, y)
move R (x, y) = (x+1, y)
move U (x, y) = (x, y+1)
move D (x, y) = (x, y-1)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (tx, ty)
  | hx == tx     && hy == ty + 2 = (tx    , ty + 1)
  | hx == tx     && hy == ty - 2 = (tx    , ty - 1)
  | hx == tx + 2 && hy == ty     = (tx + 1, ty    )
  | hx == tx - 2 && hy == ty     = (tx - 1, ty    )
  | hx == tx + 1 && hy == ty + 2 = (tx + 1, ty + 1)
  | hx == tx + 2 && hy == ty + 1 = (tx + 1, ty + 1)
  | hx == tx + 2 && hy == ty + 2 = (tx + 1, ty + 1)
  | hx == tx + 1 && hy == ty - 2 = (tx + 1, ty - 1)
  | hx == tx + 2 && hy == ty - 1 = (tx + 1, ty - 1)
  | hx == tx + 2 && hy == ty - 2 = (tx + 1, ty - 1)
  | hx == tx - 1 && hy == ty + 2 = (tx - 1, ty + 1)
  | hx == tx - 2 && hy == ty + 1 = (tx - 1, ty + 1)
  | hx == tx - 2 && hy == ty + 2 = (tx - 1, ty + 1)
  | hx == tx - 1 && hy == ty - 2 = (tx - 1, ty - 1)
  | hx == tx - 2 && hy == ty - 1 = (tx - 1, ty - 1)
  | hx == tx - 2 && hy == ty - 2 = (tx - 1, ty - 1)
  | otherwise = (tx, ty)

moveRope :: [(Int, Int)] -> Direction -> [(Int, Int)]
moveRope (h:ts) d = moveHelper newh ts
  where newh = move d h
        moveHelper h [] = [h]
        moveHelper h (t:ts) = h:moveHelper (moveTail h t) ts

ropeLog :: [[(Int, Int)]] -> Direction -> [[(Int, Int)]]
ropeLog (r:rs) d = moveRope r d:r:rs

part1 :: [(Int, Direction)] -> Int
part1 l = size $ fromList $ map last $ foldl ropeLog [replicate 2 (0, 0)] (writeOut l)

part2 :: [(Int, Direction)] -> Int
part2 l = size $ fromList $ map last $ foldl ropeLog [replicate 10 (0, 0)] (writeOut l)

main :: IO ()
main = do
  testInput <- readFile testFileName
  test2Input <- readFile test2FileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 <$> parse input "Error" mainInput)
  putStrLn $ "Part 2 Test: " ++ show (part2 <$> parse input "Error" test2Input)
  putStrLn $ "Part 2 Main: " ++ show (part2 <$> parse input "Error" mainInput)
