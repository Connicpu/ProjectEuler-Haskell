import ConnorCommon

targetNumber = 600851475143

largestPrimeFactor :: Integer -> Integer -> Integer
largestPrimeFactor x y =
  if (isFactor y x) && (isPrime y)
    then y
    else largestPrimeFactor x (y - 1)

findLargestPrimeFactor :: Integer -> Integer
findLargestPrimeFactor x = largestPrimeFactor x (intSquareRoot x)

main :: IO ()
main = do print $ "Largest prime factor: " ++ ((show . findLargestPrimeFactor) targetNumber)
