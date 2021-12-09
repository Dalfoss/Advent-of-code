module Main where

main :: IO ()
main = do
  d1_0 <- readFile "./inputs/input-1_0"
  d2_0 <- readFile "./inputs/input-2_0"
  let puzzle1Input = map read $ lines d1_0
  let puzzle2Input = lines d2_0
  putStrLn $ show $ puzzle1_0 puzzle1Input
  putStrLn $ show $ puzzle1_1 puzzle1Input
  putStrLn $ show $ puzzle2_0 puzzle2Input


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
--------------------------------------------------------------------------------

---- Day 2 ---------------------------------------------------------------------
data Direction = Up
  | Down
  | Forward
  deriving Show

data Position = Position {
    x :: Int
  , y :: Int
} deriving Show

parseCommand :: String -> (Direction, Int)
parseCommand command =
  case words command of
    ["up", mag]->  (Up, read mag)
    ["down", mag] ->  (Down, read mag)
    ["forward", mag] -> (Forward, read mag)

move :: String -> Position -> Position
move command pos =
  case parseCommand command of
    (Up, mag) -> pos{y=(y pos) - mag}
    (Down, mag) -> pos{y=(y pos) + mag}
    (Forward, mag) -> pos{x=(x pos) + mag}

puzzle2_0 :: [String] -> Int
puzzle2_0 commands = (x endPos) * (y endPos) where
  endPos = foldr move Position{x=0, y=0} commands

--------------------------------------------------------------------------------
