targetNumber = 600851475143

isFactor :: Integer -> Integer -> Bool
isFactor x y = mod y x == 0

intSquareRoot :: Integer -> Integer
intSquareRoot x = (floor . sqrt . fromIntegral) x

isPrime :: Integer -> Bool
isPrime x = primeCheck x (intSquareRoot x)

primeCheck :: Integer -> Integer -> Bool
primeCheck x 1 = True
primeCheck x y = not (isFactor y x) && (primeCheck x (y - 1))

largestPrimeFactor :: Integer -> Integer -> Integer
largestPrimeFactor x y =
  if (isFactor y x) && (isPrime y)
    then y
    else largestPrimeFactor x (y - 1)

findLargestPrimeFactor :: Integer -> Integer
findLargestPrimeFactor x = largestPrimeFactor x (intSquareRoot x)

main :: IO ()
main = do print $ "Largest prime factor: " ++ ((show . findLargestPrimeFactor) targetNumber)
