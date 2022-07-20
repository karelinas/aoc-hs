import Data.Char
import Data.List

intWords :: String -> [Int]
intWords s = map read (words s)

-- Borrowed from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) (subsequences ns)

firstValue :: [Maybe Int] -> Int
firstValue (x:xs) =
    case x of
        Just n -> n
        Nothing -> firstValue xs

evenDivisionValue :: [Int] -> Maybe Int
evenDivisionValue (a:b:_)
    | (bigger `mod` smaller) == 0 = Just (bigger `div` smaller)
    | otherwise = Nothing
    where
        bigger  = max a b
        smaller = min a b

differenceValuator :: [Int] -> Int
differenceValuator line = (maximum line) - (minimum line)

divisibilityValuator :: [Int] -> Int
divisibilityValuator = firstValue . (map evenDivisionValue) . (combinations 2)

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum lineValuator spreadsheet = sum (map lineValuator spreadsheet)

main :: IO ()
main = do
    input <- getContents
    let spreadsheet = map intWords (lines input)
    putStrLn $ show $ checksum differenceValuator spreadsheet
    putStrLn $ show $ checksum divisibilityValuator spreadsheet
