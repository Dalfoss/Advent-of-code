module Main where

import Data.Char (digitToInt)


main :: IO ()
main = do
  d1_0 <- readFile "./inputs/input-1_0"
  d2_0 <- readFile "./inputs/input-2_0"
  d3_0 <- readFile "./inputs/input-3_0"
  let puzzle1Input = map read $ lines d1_0
  let puzzle2Input = lines d2_0
  let puzzle3Input = map readToList $ lines d3_0
  putStrLn $ "Day 1, part 1: " <> (show $ puzzle1_0 puzzle1Input)
  putStrLn $ "Day 1, part 2: " <> (show $ puzzle1_1 puzzle1Input)
  putStrLn $ "Day 2, part 1: " <> (show $ puzzle2_0 puzzle2Input)
  putStrLn $ "Day 2, part 2: " <> (show $ puzzle2_1 puzzle2Input)
  putStrLn $ "Day 3, part 1: " <> (show $ puzzle3_0 puzzle3Input)
  putStrLn $ "Day 3, part 2: " <> (show $ puzzle3_1 puzzle3Input)

---- Day 1 ---------------------------------------------------------------------
puzzle1_0 :: [Int] -> Int
puzzle1_0 xs = go (head xs) (tail xs) 0 where
  go element list counter = case list of
    [] -> counter
    (x:xs) -> if x > element
                then go x xs (counter+1)
              else go x xs counter

puzzle1_0_1 :: [Int] -> Int
puzzle1_0_1 xs = length . filter (\bool -> bool == True) $ zipWith (<) (xs) (tail xs)

movingSum :: Int -> [Int] -> [Int]
movingSum window lst = reverse $ drop (window - 1) $ go window lst [] where
  go w l res = case l of
    [] -> res
    (_:_) -> go w (tail l) $ (sum . take w $ l) : res

puzzle1_1 :: [Int] -> Int
puzzle1_1 lst = (puzzle1_0 . movingSum 3) lst


---- Day 2 ---------------------------------------------------------------------
data Direction = Up
  | Down
  | Forward
  deriving Show

data Position = Position {
    x :: Int
  , y :: Int
  , aim :: Int
} deriving Show

parseCommand :: String -> (Direction, Int)
parseCommand command =
  case words command of
    ["up", mag]->  (Up, read mag)
    ["down", mag] ->  (Down, read mag)
    ["forward", mag] -> (Forward, read mag)

move0 :: Position -> String -> Position
move0 pos command =
  case parseCommand command of
    (Up, mag) -> pos{y=(y pos) - mag}
    (Down, mag) -> pos{y=(y pos) + mag}
    (Forward, mag) -> pos{x=(x pos) + mag}

move1 :: Position -> String -> Position
move1 pos command =
  case parseCommand command of
    (Up, mag) -> pos{aim=(aim pos) - mag}
    (Down, mag) -> pos{aim=(aim pos) + mag}
    (Forward, mag) -> pos{x=(x pos + mag), y=(y pos) + (aim pos * mag)}

puzzle2 :: (Position -> String -> Position) -> [String] -> Int
puzzle2 foldFunc commands = x endPos * y endPos where
  endPos = foldl foldFunc Position{x=0, y=0, aim=0} commands

puzzle2_0 :: [String] -> Int
puzzle2_0 = puzzle2 move0

puzzle2_1 :: [String] -> Int
puzzle2_1 = puzzle2 move1


---- Day 3 ---------------------------------------------------------------------

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

readToList :: String -> [Int]
readToList s = reverse $ go s [] where
  go str lst = case str of
    [] -> lst
    (x:xs) -> go xs $ (digitToInt x) : lst

majority :: Int -> [Int] -> [Int]
majority total lst = case total of
  0 -> []
  otherwise -> map (\x -> if x >= (div total 2) + (mod total 2) then 1 else 0) lst

toInt :: [Int] -> Int
toInt lst = go 0 $ reverse lst
  where go startBit bits =
          case bits of
            [] -> 0
            (x:xs) -> (x * (2^startBit)) + go (startBit + 1) (tail bits)

vectorSum :: [[Int]] -> [Int]
vectorSum lst = foldl sumVector (zeros $ length (head lst)) lst
  where sumVector = zipWith (+)

lstOf :: a -> Int -> [a]
lstOf v len = take len $ repeat v

ones :: Int -> [Int]
ones len = lstOf 1 len

zeros :: Int -> [Int]
zeros len = lstOf 0 len

xorLists :: [Int] -> [Int] -> [Int]
xorLists a b = zipWith (\x y -> if x==y then 0 else 1) a b

gamma :: [[Int]] -> [Int]
gamma lst = majority (length lst) $ vectorSum lst

epsilon :: [Int] -> [Int]
epsilon gamma = xorLists gamma (ones $ length gamma)

puzzle3_0 :: [[Int]] -> Int
puzzle3_0 lst = (toInt g) * (toInt $ epsilon g)
  where g = gamma lst

firstBitMatches :: [Int] -> [[Int]] -> [[Int]]
firstBitMatches mark = filter (\x -> head mark == head x)

ratings :: ([[Int]] -> [Int]) -> [[Int]] -> [[Int]]
ratings criteria lst = go lst [] where
  go l acc =
    if length l == 1 then prepend l
    else case m of
      [] -> prepend l
      (x:[]) -> prepend $ (firstBitMatches m l)
      (x:xs) -> go (map tail $ firstBitMatches m l) (x:acc)
    where m = criteria l
          prepend = map ((reverse acc) ++)

oxyRating :: [[Int]] -> [Int]
oxyRating lst = head $ ratings gamma lst

co2ScrubRating :: [[Int]] -> [Int]
co2ScrubRating lst = head $ ratings (epsilon . gamma) lst

puzzle3_1 :: [[Int]] -> Int
puzzle3_1 lst = (toInt $ oxyRating lst) * (toInt $ co2ScrubRating lst)
