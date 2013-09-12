import Data.List

coins :: [Integer]
coins = [ 1, 2, 5, 10, 20, 50, 100, 200 ]
target :: Integer
target = 200

countCoins :: Integer -> Integer
countCoins x =
  countCoins' (length coins) x
  where
    countCoins' 1 _ = 1
    countCoins' n x = sum $ map addCoin [0..x `div` coins !! pred n]
      where addCoin k = countCoins' (pred n) (x - k * coins !! pred n)

problem_31 :: Integer
problem_31 = countCoins 200

main :: IO ()
main = putStrLn $ "Ways to have coins total up to 200p: " ++ (show problem_31)
