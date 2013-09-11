import ConnorCommon (divisors, index)
import Debug.Trace (trace)

limit :: Int
limit = 28124

abundant :: Int -> Bool
abundant n = (sum.divisors $ n) > n

abundantNumbers :: [Int]
abundantNumbers = [ x | x <- [1..limit], abundant x ]

abundantDiffs :: Int -> [Int]
abundantDiffs n = map (n-) $ takeWhile (< n `div` 2) abundantNumbers

nonAbundant :: Int -> Bool
nonAbundant n = not $ any (abundant) (abundantDiffs n)

problem_23 :: Integer
problem_23 = sum $ map toInteger $ filter (nonAbundant) [1..limit]

main :: IO ()
main = putStrLn $ "Sum of Non-SuperAbundant numbers: " ++ (show problem_23)

