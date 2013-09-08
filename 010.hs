import ConnorCommon (primesTo)

main :: IO ()
main = putStrLn $ "Sum of all primes below 2,000,000: " ++ (show $ sum (primesTo 2000000))
