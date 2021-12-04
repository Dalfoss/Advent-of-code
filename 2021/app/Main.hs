module Main where

main :: IO ()
main = do
 d1_0 <- readFile "./inputs/input-1_0"
 putStrLn $ show $ puzzle1_0 . map read $ lines d1_0

puzzle1_0 :: [Int] -> Int
puzzle1_0 xs = go (head xs) (tail xs) 0 where
  go element list counter = case list of
    [] -> counter
    (x:xs) -> if x > element
                then go x xs (counter+1)
              else go x xs counter

puzzle1_0_1 :: [Int] -> Int
puzzle1_0_1 xs = length . filter (\bool -> bool == True) $ zipWith (<) (xs) (tail xs)
