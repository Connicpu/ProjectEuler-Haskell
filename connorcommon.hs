module ConnorCommon (isPrime, intSquareRoot, isFactor) where
  isFactor :: Integer -> Integer -> Bool
  isFactor x y = mod y x == 0

  intSquareRoot :: Integer -> Integer
  intSquareRoot x = (floor . sqrt . fromIntegral) x

  isPrime :: Integer -> Bool
  isPrime x = primeCheck x (intSquareRoot x)

  primeCheck :: Integer -> Integer -> Bool
  primeCheck x 1 = True
  primeCheck x y = not (isFactor y x) && (primeCheck x (y - 1))
