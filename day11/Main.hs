{-# LANGUAGE FlexibleInstances #-} -- allows Show for Integer -> Integer, debugging

import Text.Parsec.String (GenParser)
import Text.Parsec (many, (<|>), parse, sepBy, try)
import Text.Parsec.Char (endOfLine, char, string, digit)
import Data.List (sortBy)

testFileName = "test.txt"
inputFileName = "input.txt"

data Monkey = Monkey { items :: [Integer]
                     , operation :: Integer -> Integer
                     , divisibleBy :: Integer
                     , ifTrue :: Int
                     , ifFalse :: Int
                     } deriving Show

instance Show (Integer -> Integer) where
  show f = "1 -> " ++ show (f 1) ++ ", 2 -> " ++ show (f 2) ++ ", 3 -> " ++ show (f 3) ++ " etc."

input :: GenParser Char st [Monkey]
input = many monkey
  where monkey = do
          string "Monkey " >> many digit >> char ':' >> endOfLine
          string "  Starting items: "
          items <- many digit `sepBy` string ", "
          endOfLine
          string "  Operation: new = "
          n1 <- old <|> number
          op <- try plus <|> try times
          n2 <- old <|> number
          endOfLine
          string "  Test: divisible by "
          divBy <- many digit
          endOfLine
          string "    If true: throw to monkey "
          trueMonkey <- many digit
          endOfLine
          string "    If false: throw to monkey "
          falseMonkey <- many digit
          many endOfLine
          return Monkey { items = map read items
                        , operation = \x -> op (n1 x) (n2 x)
                        , divisibleBy = read divBy
                        , ifTrue = read trueMonkey
                        , ifFalse = read falseMonkey
                        }
        old = string "old" >> return id
        number = do
          n <- many digit
          return $ const (read n)
        plus = string " + " >> return (+)
        times = string " * " >> return (*)

throw :: Int -> [Monkey] -> Maybe [Monkey]
throw n ms
  | null $ items (ms !! n) = Nothing
  | otherwise = Just $ mi2 ++ (m3:mt2) where
      (mi, m:mt) = splitAt n ms
      i:is = items m
      m1 = m { items = is }
      ms1 = mi ++ (m1:mt)
      wl = operation m i `div` 3
      newn = if wl `mod` divisibleBy m == 0 then ifTrue m else ifFalse m
      (mi2, m2:mt2) = splitAt newn ms1
      m3 = m2 { items = items m2 ++ [wl] }

throwNew :: Int -> [Monkey] -> Maybe [Monkey]
throwNew n ms
  | null $ items (ms !! n) = Nothing
  | otherwise = Just $ mi2 ++ (m3:mt2) where
      (mi, m:mt) = splitAt n ms
      i:is = items m
      m1 = m { items = is }
      ms1 = mi ++ (m1:mt)
      wl = operation m i `mod` foldl lcm 1 (map divisibleBy ms)
      newn = if wl `mod` divisibleBy m == 0 then ifTrue m else ifFalse m
      (mi2, m2:mt2) = splitAt newn ms1
      m3 = m2 { items = items m2 ++ [wl] }

turn :: Int -> [Monkey] -> (Integer, [Monkey])
turn n ms = turnHelper (0, ms) where
  turnHelper (k, monkeys) = case throw n monkeys of
    Nothing -> (k, monkeys)
    Just newms -> turnHelper (k+1, newms)

turnNew :: Int -> [Monkey] -> (Integer, [Monkey])
turnNew n ms = turnHelper (0, ms) where
  turnHelper (k, monkeys) = case throwNew n monkeys of
    Nothing -> (k, monkeys)
    Just newms -> turnHelper (k+1, newms)

doRound :: [Monkey] -> ([Integer], [Monkey])
doRound ms = roundHelper [0..(length ms - 1)] ([], ms) where
  roundHelper [] (ts, ms) = (ts, ms)
  roundHelper (n:ns) (ts, ms) = roundHelper ns (k:ts, newms)
    where (k, newms) = turn n ms

doRoundNew :: [Monkey] -> ([Integer], [Monkey])
doRoundNew ms = roundHelper [0..(length ms - 1)] ([], ms) where
  roundHelper [] (ts, ms) = (ts, ms)
  roundHelper (n:ns) (ts, ms) = roundHelper ns (k:ts, newms)
    where (k, newms) = turnNew n ms

rounds :: Integer -> [Monkey] -> ([Integer], [Monkey])
rounds 0 ms = (replicate (length ms) 0, ms)
rounds n ms = (zipWith (+) ks ks2, finalms) where
  (ks, newms) = doRound ms
  (ks2, finalms) = rounds (n-1) newms

roundsNew :: Integer -> [Monkey] -> ([Integer], [Monkey])
roundsNew 0 ms = (replicate (length ms) 0, ms)
roundsNew n ms = (zipWith (+) ks ks2, finalms) where
  (ks, newms) = doRoundNew ms
  (ks2, finalms) = roundsNew (n-1) newms

part1 :: [Monkey] -> Integer
part1 ms = product $ take 2 $ sortBy (flip compare) (fst $ rounds 20 ms)

part2 :: [Monkey] -> Integer
part2 ms = product $ take 2 $ sortBy (flip compare) (fst $ roundsNew 10000 ms)

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 <$> parse input "Error" mainInput)
  putStrLn $ "Part 1 Test: " ++ show (part2 <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part2 <$> parse input "Error" mainInput)
