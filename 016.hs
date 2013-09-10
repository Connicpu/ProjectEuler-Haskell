two :: Integer
two = 2

twoTo1000 :: Integer
twoTo1000 = two ^ 1000

problem_16 :: Integer
problem_16 = sum [ read [x] | x <- (show twoTo1000) ]

main :: IO ()
main = putStrLn $ "Sum of the digits of 2^1000: " ++ (show problem_16)
