factorial :: Integer -> Integer
factorial n = product [1..n]

problem_20 :: Integer
problem_20 = sum [ read [x] | x <- (show.factorial $ 100) ]

main :: IO ()
main = putStrLn $ "Sum of digits of 100!: " ++ (show problem_20)
