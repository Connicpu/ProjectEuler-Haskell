import Data.List
import Data.Ord

consecutiveProduct :: [Integer] -> Int -> Integer -> Integer
consecutiveProduct set x n = product $ (take x . drop (fromIntegral n :: Int)) set

greatestConsecutiveProduct :: Int -> [Integer] -> Integer
greatestConsecutiveProduct n set =
  last (sortBy (comparing (consecutiveProduct set n)) [0..l])
  where l = fromIntegral ((length set) - n) :: Integer

charsToInts :: [Char] -> [Integer]
charsToInts chars = [ (read [x]) | x <- chars ]

indexOfGreatest :: [Char] -> Int
indexOfGreatest digits = fromIntegral $ greatestConsecutiveProduct 5 (charsToInts digits)

greatestDigits :: [Char] -> [Char]
greatestDigits digits = (take 5 . drop (indexOfGreatest digits)) digits

main :: IO ()
main = do
  digits <- readFile "008_chars.txt"
  putStrLn $ "Greatest consecutive digits: " ++ (show $ product $ charsToInts $ greatestDigits digits)

