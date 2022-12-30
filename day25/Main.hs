import Text.Parsec.String (GenParser)
import Text.Parsec (many, (<|>), parse, sepBy, try)
import Text.Parsec.Char (endOfLine, char, string, digit)

testFileName = "test.txt"
inputFileName = "input.txt"

data SnafuDigit = NTwo | NOne | Zero | One | Two

input :: GenParser Char st [[SnafuDigit]]
input = many snafu where
  snafu = do
    ds <- many snafuDigit
    endOfLine
    return $ reverse ds
  snafuDigit = snafuNTwo <|> snafuNOne <|> snafuZero <|> snafuOne <|> snafuTwo
  snafuNTwo = char '=' >> return NTwo
  snafuNOne = char '-' >> return NOne
  snafuZero = char '0' >> return Zero
  snafuOne = char '1' >> return One
  snafuTwo = char '2' >> return Two

snafuToChar :: SnafuDigit -> Char
snafuToChar NTwo = '='
snafuToChar NOne = '-'
snafuToChar Zero = '0'
snafuToChar One = '1'
snafuToChar Two = '2'

snafuToString :: [SnafuDigit] -> String
snafuToString = reverse . map snafuToChar


digitValue :: SnafuDigit -> Int
digitValue NTwo = -2
digitValue NOne = -1
digitValue Zero = 0
digitValue One = 1
digitValue Two = 2

-- I'll represent the SNAFU numbers as list of digits where the ones form the head
snafuToInt :: [SnafuDigit] -> Int
snafuToInt [] = 0
snafuToInt (d:ds) = digitValue d + 5 * snafuToInt ds

intToSnafu :: Int -> [SnafuDigit]
intToSnafu n = d:ds where
  r = n `mod` 5
  d
    | r == 0 = Zero
    | r == 1 || r == -4 = One
    | r == 2 || r == -3 = Two
    | r == 3 || r == -2 = NTwo
    | r == 4 || r == -1 = NOne
    | otherwise = undefined
  newn = (n - digitValue d) `div` 5
  ds = if newn == 0 then [] else intToSnafu newn

part1 :: [[SnafuDigit]] -> String
part1 = snafuToString . intToSnafu . sum . map snafuToInt

-- no part2 today

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 <$> parse input "Error" mainInput)
  return ()
