import System.Environment

sumOfMultiples :: Integer -> Integer
sumOfMultiples n = sum [ x | x <- [1..n], (x `mod` 3) == 0 || (x `mod` 5) == 0 ]

main :: IO ()
main = putStrLn $ "Sum: " ++ (show $ sumOfMultiples 999)
