import Text.Parsec.String (GenParser)
import Text.Parsec (many, (<|>), parse)
import Text.Parsec.Char (endOfLine, char, string, digit)
import Data.List.Split (chunksOf)

testFileName = "test.txt"
inputFileName = "input.txt"

data Instr = Noop | Addx Int

input :: GenParser Char st [Instr]
input = many (noop <|> addx)
  where noop = string "noop" >> endOfLine >> return Noop
        addx = do
          string "addx "
          n <- negInt <|> posInt
          endOfLine
          return $ Addx n
        negInt = do
          char '-'
          n <- many digit
          return $ -read n
        posInt = do
          n <- many digit
          return $ read n

regs :: [Instr] -> [Int]
regs = regHelper 1 where
  regHelper _ [] = []
  regHelper x (Noop:is) = x:regHelper x is
  regHelper x ((Addx n):is) = x:x:regHelper (x+n) is

signalStrength :: [Int] -> Int -> Int
signalStrength xs c = c * (xs !! (c-1))

part1 :: [Instr] -> Int
part1 is = sum $ map (signalStrength xs) [20, 60, 100, 140, 180, 220]
  where xs = regs is

renderLine :: [Int] -> String
renderLine xs = zipWith (\x y -> if x - y <= 1 && y - x <= 1 then '#' else '.') xs [0..40]

part2 :: [Instr] -> String
part2 is = unlines ls
  where xs = chunksOf 40 $ regs is
        ls = map renderLine xs

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 <$> parse input "Error" mainInput)
  putStrLn ("Part 2 Test: \n" ++ either (const "") part2 (parse input "Error" testInput))
  putStrLn ("Part 2 Main: \n" ++ either (const "") part2 (parse input "Error" mainInput))
