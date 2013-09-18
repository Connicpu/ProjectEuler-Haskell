import Data.List

range :: [Integer]
range = [2..100]

problem_29 :: Int
problem_29 = length.nub $ [ a^b | a <- range, b <- range ]

main :: IO ()
main = putStrLn $ show problem_29
