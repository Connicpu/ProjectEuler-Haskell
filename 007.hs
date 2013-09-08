import ConnorCommon

nthPrime :: Int -> Integer
nthPrime = ([ x | x <- [1..], isPrime x ] !!)

main :: IO ()
main = do
  print "Finding the 10001st prime..."
  print $ "10001st Prime: " ++ show (nthPrime 10001)
