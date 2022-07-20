import Data.Char

pairs :: Int -> [a] -> [(a, a)]
pairs offset xs = zip xs xsRotated
    where xsRotated = drop offset (cycle xs)

pairValue :: (Int, Int) -> Int
pairValue (a, b)
    | (a == b) = a
    | otherwise = 0

halfpoint :: [a] -> Int
halfpoint xs = (length xs) `div` 2

solution :: Int -> ([Int] -> Int)
solution offset = sum . (map pairValue) . (pairs offset)

main :: IO ()
main = do
    input <- getLine
    let captcha = map digitToInt input
    putStrLn $ show $ solution 1 captcha
    putStrLn $ show $ solution (halfpoint captcha) captcha
