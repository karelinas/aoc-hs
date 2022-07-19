import Data.Char
import Data.List

intWords :: String -> [Int]
intWords s = map read (words s)

differenceOfExtremes :: [Int] -> Int
differenceOfExtremes line = maximum(line) - minimum(line)

-- Borrowed from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) (subsequences ns)

evenDivisionValue :: [Int] -> Int
evenDivisionValue (a:b:xs) = if (max a b) `mod` (min a b) == 0
                             then (max a b) `div` (min a b)
                             else 0

evenDivision :: [Int] -> Int
evenDivision line = sum (map evenDivisionValue (combinations 2 line))

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum lineValuator spreadsheet = sum(map lineValuator spreadsheet)



main :: IO ()
main = do
    input <- getContents
    let spreadsheet = map intWords (lines input)
    putStrLn (show (checksum differenceOfExtremes spreadsheet))
    putStrLn (show (checksum evenDivision spreadsheet))