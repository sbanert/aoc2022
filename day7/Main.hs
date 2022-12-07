import Text.Parsec.String (GenParser)
import Text.Parsec (many, (<|>), parse)
import Text.Parsec.Char (string, endOfLine, letter, char, digit)

testFileName = "test.txt"
inputFileName = "input.txt"

-- Result of input parsing
data Line = Comm Comm | Cont Cont         deriving Show -- Line in the input file
data Comm = CD Dest | LS                  deriving Show -- Command: either cd or ls
data Dest = In String | Out | Root        deriving Show -- Destination for cd
data Cont = Dir String | File Int String  deriving Show -- Files or directories

input :: GenParser Char st [Line]
input = many $ commandLine <|> dirLine <|> fileLine where
  commandLine = string "$ " >> (cdCommand <|> lsCommand)
  cdCommand = string "cd " >> (root <|> up <|> down)
  root = string "/" >> endOfLine >> return (Comm $ CD Root)
  up = string ".." >> endOfLine >> return (Comm $ CD Out)
  down = do
    name <- many (letter <|> char '.')
    endOfLine
    return $ Comm $ CD $ In name
  lsCommand = string "ls" >> endOfLine >> return (Comm LS)
  dirLine = do
    string "dir "
    name <- many (letter <|> char '.')
    endOfLine
    return $ Cont $ Dir name
  fileLine = do
    size <- many digit
    string " "
    name <- many (letter <|> char '.')
    endOfLine
    return $ Cont $ File (read size) name

-- I guess, Unknown is unnecessary, and the trees could just be initialised with empty lists. Oh well ...
data DirectoryTree = Unknown | DTree [(String, Int)] [(String, DirectoryTree)] deriving Show

startingTree :: DirectoryTree
startingTree = Unknown

apply :: (DirectoryTree, [String]) -> Line -> (DirectoryTree, [String])
apply (t, ds) (Comm (CD (In s))) = (t, ds ++ [s])
apply (t, ds) (Comm (CD Out)) = (t, init ds)
apply (t, ds) (Comm (CD Root)) = (t, [])
apply (t, ds) (Comm LS) = (makeKnown t ds, ds)
apply (t, ds) (Cont c) = (insert c t ds, ds)

makeKnown :: DirectoryTree -> [String] -> DirectoryTree
makeKnown t [] = DTree [] []
makeKnown (DTree fs (e:es)) (d:ds)
  | fst e == d = DTree fs ((d, makeKnown (snd e) ds):es)
  | otherwise = makeKnown (DTree fs (es ++ [e])) (d:ds) -- really dirty, I guess

insert :: Cont -> DirectoryTree -> [String] -> DirectoryTree
insert (Dir name) (DTree fs ds) [] = DTree fs ((name, Unknown):ds)
insert (File size name) (DTree fs ds) [] = DTree ((name, size):fs) ds
insert c (DTree fs (e:es)) (d:ds)
  | fst e == d = DTree fs ((d, insert c (snd e) ds):es)
  | otherwise = insert c (DTree fs (es ++ [e])) (d:ds)

directorySize :: DirectoryTree -> Int
directorySize Unknown = 0
directorySize (DTree fs ds) = sum (map snd fs) + sum (map (directorySize . snd) ds)
 
part1 :: DirectoryTree -> Int
part1 Unknown = 0
part1 (DTree fs ds)
  | directorySize (DTree fs ds) <= 100000 = directorySize (DTree fs ds) + sum (map (part1 . snd) ds)
  | otherwise = sum (map (part1 . snd) ds)

totalSpace = 70000000
requiredSpace = 30000000

part2 :: DirectoryTree -> Int
part2 d = part2Helper minSize d 
  where
    part2Helper _ Unknown = totalSpace
    part2Helper minSize (DTree fs ds) = if s >= minSize then minimum (s:map (part2Helper minSize. snd) ds) else totalSpace
      where s = directorySize (DTree fs ds)
    unusedSpace = totalSpace - directorySize d
    minSize = requiredSpace - unusedSpace

treeFromLines :: [Line] -> DirectoryTree
treeFromLines ls = fst $ foldl apply (Unknown, []) ls

main :: IO ()
main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  putStrLn $ "Part 1 Test: " ++ show (part1 . treeFromLines <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (part1 . treeFromLines <$> parse input "Error" mainInput)
  putStrLn $ "Part 2 Test: " ++ show (part2 . treeFromLines <$> parse input "Error" testInput)
  putStrLn $ "Part 2 Main: " ++ show (part2 . treeFromLines <$> parse input "Error" mainInput)
