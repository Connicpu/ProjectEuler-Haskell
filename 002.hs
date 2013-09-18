import System.Environment

fib :: Int -> Integer
fib = (map fib' [0..] !!)
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n-2) + fib (n-1)

fibonacci :: [Integer]
fibonacci = map fib [1..]

evenFibonacciSum :: Integer -> [Char]
evenFibonacciSum n = show (sum $ takeWhile (<=n) [ x | x <- fibonacci, (x `mod` 2) == 0 ])

main :: IO ()
main = do print ("Sum: " ++ (evenFibonacciSum 4000000))
