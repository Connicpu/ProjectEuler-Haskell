module Main where

import Control.Monad
import Data.List

combs :: (Eq b) => Int -> [b] -> [([b], [b])]
combs 0 xs = [([], xs)]
combs n xs = [(y:ys, rest) | y <- xs, (ys, rest) <- combs (n-1) (delete y xs)]

listToNumber :: (Integral a) => [a] -> a
listToNumber = foldl' (\a b -> 10*a + b) 0

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

explode :: (Integral a) => a -> [a]
explode = unfoldr (\a -> if a == 0 then Nothing else (Just . swap $ quotRem a 10))

pandigitals :: [Integer]
pandigitals =
  nub $ do
    (beginning, end) <- combs 5 [1..9]
    n <- [1, 2]
    let (a, b) = splitAt n beginning
        res = listToNumber a * listToNumber b
    guard $ sort (explode res) == end
    return res

main :: IO ()
main = putStrLn $ "Sum of pandigitals: " ++ (show.sum $ pandigitals)

