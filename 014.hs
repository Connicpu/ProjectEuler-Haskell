import Data.Ord
import Data.List

collatzChain :: [Integer] -> [Integer]
collatzChain a = 
  if (head a) == 1
    then a
    else collatzChain $ (nextCollatz $ head a) : a

nextCollatz :: Integer -> Integer
nextCollatz n =
  if n `mod` 2 == 0
    then n `div` 2
    else 3*n + 1

chainsUnder :: Integer -> [(Integer, Int)]
chainsUnder n = [ (x, length $ collatzChain [x]) | x <- [1..(n-1)] ]

longestChainUnder :: Integer -> (Integer, Int)
longestChainUnder n = last $ sortBy (comparing snd) (chainsUnder n)

startOfLongestUnderOneMillion :: Integer
startOfLongestUnderOneMillion = fst $ longestChainUnder 1000000

main :: IO ()
main = putStrLn $ "Starting piece of longest collatz chain under 1000000: " ++ (show startOfLongestUnderOneMillion)
