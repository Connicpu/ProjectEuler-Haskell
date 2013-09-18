import Data.Ratio
import Data.List.Ordered

allFractions :: [Rational]
allFractions = [ 1 / n | n <- [1..1000] ]

getRecurringCycle :: Rational -> String
getRecurringCycle x =
  map (head.show) $ reverse chain
  where
    longDivision n d xs =
      if (seenRemainder r xs) || (r == 0)
        then (m, r):xs
        else longDivision (r * 10) d ((m, r):xs)
      where 
        r = n `rem` d
        m = n `div` d
    seenRemainder n xs = any ((n==).snd) xs
    buildRepeating xs =
      if r == 0
        then []
        else [(fst $ head xs)] ++ (map fst (takeWhile ((/=r).snd) (tail xs)))
      where r = snd.head $ xs
    chain = buildRepeating $ longDivision (numerator x) (denominator x) []

fractionLengths :: [[Integer]]
fractionLengths = map (\a -> [(toInteger.length $ getRecurringCycle a), denominator a]) allFractions

problem_26 :: Integer
problem_26 = (!! 1).last $ sort fractionLengths

main :: IO ()
main = putStrLn $ show problem_26
