import Data.Char

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n (x:xs) = if n <= 0 || null xs
                  then (x:xs)
                  else rotate (n - 1) (xs ++ [x])

pairs :: Int -> [a] -> [(a, a)]
pairs n xs = zip xs (rotate n xs)

pairSum :: (Int, Int) -> Int
pairSum (a, b) = if a == b then a else 0

halfpoint :: [a] -> Int
halfpoint xs = (length xs) `div` 2

partOneSolution :: [Int] -> Int
partOneSolution digits = sum (map pairSum (pairs 1 digits))

partTwoSolution :: [Int] -> Int
partTwoSolution digits = sum (map pairSum (pairs (halfpoint digits) digits))

main :: IO ()
main = do
    input <- getLine
    let digits = map digitToInt input
    putStrLn (show (partOneSolution digits))
    putStrLn (show (partTwoSolution digits))
