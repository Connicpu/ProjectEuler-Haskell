import ConnorCommon (divisors)

d :: Integer -> Integer
d n = sum.divisors $ n

isAmicable :: (Integer, Integer) -> Bool
isAmicable (a, b) = a /= b && d (a) == b && d (b) == a

amicableNumbers :: [Integer]
amicableNumbers = [ fst x | x <- [ (x, (d x)) | x <- [1..] ], isAmicable x ]

problem_21 :: Integer
problem_21 = sum.takeWhile (<10000) $ amicableNumbers

main :: IO ()
main = putStrLn $ "Sum of all amicable numbers under 10000: " ++ (show problem_21)
