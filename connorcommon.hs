module ConnorCommon (
  isPrime, intSquareRoot,
  isFactor, primes, primesTo, 
  third, divisors, index) where
  
  import Data.List
  import qualified Data.Numbers.Primes as Primes

  index :: Eq a => a -> [a] -> Integer
  index item list = maybe (-1) toInteger (elemIndex item list)

  divisors :: Integral a => a -> [a]
  divisors n = [ x | x <- [1..mid], x `isFactor` n ]
    where mid = n `div` 2

  third :: (a, b, c) -> c
  third (_, _, c) = c

  isFactor :: Integral a => a -> a -> Bool
  isFactor x y = y `mod` x == 0

  intSquareRoot :: Integer -> Integer
  intSquareRoot = ceiling.sqrt.toDub
    where
      toDub :: Integer -> Double
      toDub = fromIntegral

  isPrime :: Integer -> Bool
  isPrime x = length ([ y | y <- [2..mid], isFactor y x ]) == 0
    where mid = intSquareRoot x

  primes :: [Integer]
  primes = Primes.primes

  primesTo :: Integer -> [Integer]
  primesTo n = takeWhile (<n) primes

