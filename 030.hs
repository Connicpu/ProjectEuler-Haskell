numbers :: [Integer]
numbers = [2..400000]

fifthPowSum :: Integer -> Bool
fifthPowSum n = (sum [ (read [i])^(5::Integer) | i <- (show n) ]) == n

problem_30 :: Integer
problem_30 = sum $ filter fifthPowSum numbers

main :: IO ()
main = putStrLn $ show problem_30
