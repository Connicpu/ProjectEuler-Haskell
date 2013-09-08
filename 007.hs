isFactor :: Integer -> Integer -> Bool
isFactor x y = mod y x == 0

intSquareRoot :: Integer -> Integer
intSquareRoot x = (floor . sqrt . fromIntegral) x

isPrime :: Integer -> Bool
isPrime x = primeCheck x (intSquareRoot x)

primeCheck :: Integer -> Integer -> Bool
primeCheck x 1 = True
primeCheck x y = not (isFactor y x) && (primeCheck x (y - 1))

nthPrime :: Int -> Integer
nthPrime = ([ x | x <- [1..], isPrime x ] !!)

main :: IO ()
main = do
  print "Finding the 10001st prime..."
  print $ "10001st Prime: " ++ show (nthPrime 10001)
