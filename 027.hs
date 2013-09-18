module Main where

problem_27 :: Integer
problem_27 = -(a * b)
  where
    n = 1000
    m = head $ filter quadGTN [1..]
    quad x = x^(2::Integer) - x + (41::Integer)
    quadGTN x = quad x > n
    c = m - 1
    a = quad c
    b = 2*c - 1

main :: IO ()
main = putStrLn $ show problem_27
