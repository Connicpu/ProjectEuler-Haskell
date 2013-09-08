import Data.Numbers.Primes
import Data.List

triangleNumbers = scanl1 (+) [1..]
nDivisors n = product $ map ((+1) . length) (group (primeFactors n))
answer = head $ filter ((> 500) . nDivisors) triangleNumbers

main :: IO ()
main = putStrLn $ "First triangle number to have over 500 divisors: " ++ (show answer)

