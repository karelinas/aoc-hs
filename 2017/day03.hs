import Data.Complex

origin :: RealFloat a => Complex a
origin = 0.0 :+ 0.0

right :: RealFloat a => Complex a
right = 1.0 :+ 0.0

i :: RealFloat a => Complex a
i = 0.0 :+ 1.0

manhattan :: RealFloat a => Complex a -> Int
manhattan c = round ((abs (realPart c)) + (abs (imagPart c)))

{-
  spiralSteps traces 'n' steps around a spiral and returns the final position.

  n = number of steps to take around the spiral
  l = length of a side, increasing as we go outward in the spiral
  d = direction
  p = position
-}
spiralSteps :: RealFloat a => Int -> Int -> Complex a -> Complex a -> Complex a
spiralSteps n l d p
    | n <= l       = p + (n' * d)
    | n <= (2 * l) = p + (l' * d) + ((n' - l') * (d * i))
    | otherwise    = spiralSteps (n - (2 * l)) (l + 1) (d * i * i) (p + (l' * d) + (l' * d * i))
    where
        n' = fromIntegral n :: RealFloat a => Complex a
        l' = fromIntegral l :: RealFloat a => Complex a

spiralDistance :: Int -> Int
spiralDistance n = manhattan (spiralSteps (n - 1) 1 right origin)

puzzleInput :: Int
puzzleInput = 347991

main :: IO ()
main = do
    putStrLn $ show $ spiralDistance puzzleInput