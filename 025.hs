fib :: Int -> Integer
fib = (map fib' [0..] !!)
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 2) + fib (n - 1)

fibonacci :: [(Integer, Int)]
fibonacci = [ (fib n, n) | n <- [1..] ]

hasDigits :: Integer -> ((Integer, Int) -> Bool)
hasDigits n x = (toInteger.length.show.fst $ x) >= n

problem_25 :: Int
problem_25 = snd.head $ dropWhile (not.(hasDigits 1000)) fibonacci

main :: IO ()
main = putStrLn $ "First fibonacci with 1000 digits: " ++ (show problem_25)
