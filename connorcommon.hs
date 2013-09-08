module ConnorCommon (isPrime, intSquareRoot, isFactor, primes, primesTo) where
  import Data.List
  import qualified Data.Map as M

  isFactor :: Integer -> Integer -> Bool
  isFactor x y = y `mod` x == 0

  intSquareRoot :: Integer -> Integer
  intSquareRoot x = (floor . sqrt . fromIntegral) x

  isPrime :: Integer -> Bool
  isPrime x = length ([ y | y <- [2..mid], isFactor y x ]) == 0
    where mid = intSquareRoot x

  lessThanFilter x y = (x > y)
  primes = primesMPE
  primesTo n = takeWhile (lessThanFilter n) primes
   
  primesMPE :: [Integer]
  primesMPE = 2:mkPrimes 3 M.empty prs 9   -- postponed addition of primes into map;
    where                                  -- decoupled primes loop feed 
      prs = 3:mkPrimes 5 M.empty prs 9
      mkPrimes n m ps@ ~(p:t) q = case (M.null m, M.findMin m) of
          (False, (n', skips)) | n == n' ->
               mkPrimes (n+2) (addSkips n (M.deleteMin m) skips) ps q
          _ -> if n<q
               then    n : mkPrimes (n+2)  m                  ps q
               else        mkPrimes (n+2) (addSkip n m (2*p)) t (head t^2)
   
      addSkip n m s = M.alter (Just . maybe [s] (s:)) (n+s) m
      addSkips = foldl' . addSkip

