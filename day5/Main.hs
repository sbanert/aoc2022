import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.List (transpose)

testFileName = "test.txt"
inputFileName = "input.txt"

applyInstr :: [[Char]] -> (Int, Int, Int)  -> [[Char]]
applyInstr ss (n, o, d) = ss2
  where
    (h, so:t) = splitAt (o-1) ss
    (mv, so1) = splitAt n so
    ss1 = h ++ so1:t
    (h1, sd:t1) = splitAt (d-1) ss1
    ss2 = h1 ++ (reverse mv ++ sd):t1

applyInstr2 :: [[Char]] -> (Int, Int, Int)  -> [[Char]]
applyInstr2 ss (n, o, d) = ss2
  where
    (h, so:t) = splitAt (o-1) ss
    (mv, so1) = splitAt n so
    ss1 = h ++ so1:t
    (h1, sd:t1) = splitAt (d-1) ss1
    ss2 = h1 ++ (mv ++ sd):t1

part1 :: ([[Char]], [(Int, Int, Int)]) -> [Char]
part1 (cs, is) = map head $ foldl applyInstr cs is

part2 :: ([[Char]], [(Int, Int, Int)]) -> [Char]
part2 (cs, is) = map head $ foldl applyInstr2 cs is

input :: GenParser Char st ([[Char]], [(Int, Int, Int)])
input = do
  ss <- boxLines
  instructions <- many instructionLine
  return (ss, instructions)

boxLines :: GenParser Char st [[Char]]
boxLines = do
  bls <- manyTill boxLine (try interlude)
  return $ stacks bls

boxLine :: GenParser Char st [Maybe Char]
boxLine = do
  b <- box
  bs <- many (char ' ' >> box)
  endOfLine
  return (b:bs)

box :: GenParser Char st (Maybe Char)
box = justBox <|> noBox

justBox :: GenParser Char st (Maybe Char)
justBox = do
  char '['
  c <- letter
  char ']'
  return $ Just c

noBox = do
  string "   "
  return Nothing
  
interlude :: GenParser Char st ()
interlude = do
  string " 1 "
  many (digit <|> space <|> endOfLine)
  return ()

instructionLine :: GenParser Char st (Int, Int, Int)
instructionLine = do
  string "move "
  n <- many1 digit
  string " from "
  o <- many1 digit
  string " to "
  d <- many1 digit
  endOfLine
  return (read n, read o, read d)
  
stacks :: [[Maybe Char]] -> [[Char]]
stacks cs = map skipMaybe $ transpose cs

skipMaybe :: [Maybe Char] -> [Char]
skipMaybe [] = []
skipMaybe (Nothing:xs) = skipMaybe xs
skipMaybe ((Just a):xs) = a:skipMaybe xs

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 <$> parse input "Error" mainInput)
  putStrLn $ "Part 2 Test: " ++ show (part2 <$> parse input "Error" testInput)
  putStrLn $ "Part 2 Main: " ++ show (part2 <$> parse input "Error" mainInput)
