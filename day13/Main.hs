import Text.Parsec.String (GenParser)
import Text.Parsec (many, many1, (<|>), parse, sepBy)
import Text.Parsec.Char (endOfLine, char, digit)
import Data.List (sort)

testFileName = "test.txt"
inputFileName = "input.txt"

data Packet = Number Int | PList [Packet] deriving (Show, Eq)

input :: GenParser Char st [(Packet, Packet)]
input = many packetPair where
  packetPair = do
    p1 <- packet
    endOfLine
    p2 <- packet
    many endOfLine
    return (p1, p2)
  packet = plist <|> pnumber
  pnumber = do
    n <- many1 digit
    return $ Number (read n)
  plist = do
    char '['
    ps <- packet `sepBy` char ','
    char ']'
    return $ PList ps

instance Ord Packet where
  compare (Number a) (Number b) = compare a b
  compare (PList []) (PList []) = EQ
  compare (PList []) (PList (_:_)) = LT
  compare (PList (_:_)) (PList []) = GT
  compare (PList (a:as)) (PList (b:bs)) = if compare a b == EQ then compare as bs else compare a b
  compare (Number a) (PList bs) = compare (PList [Number a]) (PList bs)
  compare (PList as) (Number b) = compare (PList as) (PList [Number b])

part1 :: [(Packet, Packet)] -> Int
part1 pps = sum $ map fst $ filter (\(_, (p1, p2)) -> p1 < p2) $ zip [1..] pps

pList :: [(Packet, Packet)] -> [Packet]
pList [] = []
pList ((p1, p2):ps) = p1:p2:pList ps

d1 :: Packet
d1 = PList [PList [Number 2]]

d2 :: Packet
d2 = PList [PList [Number 6]]

part2 :: [(Packet, Packet)] -> Int
part2 pps = product $ map fst $ filter (\ (_, p) -> p == d1 || p == d2) $ zip [1..] $ sort (d1:d2:pList pps)

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 <$> parse input "Error" mainInput)
  putStrLn $ "Part 2 Test: " ++ show (part2 <$> parse input "Error" testInput)
  putStrLn $ "Part 2 Main: " ++ show (part2 <$> parse input "Error" mainInput)
