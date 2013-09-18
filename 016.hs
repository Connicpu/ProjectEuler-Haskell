problem_16 :: Integer
problem_16 = sum [ read [x] | x <- (show $ two^oneThousand) ]
  where
    two = 2 :: Integer
    oneThousand = 1000 :: Integer

main :: IO ()
main = putStrLn $ "Sum of the digits of 2^1000: " ++ (show problem_16)
