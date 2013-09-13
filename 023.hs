import ConnorCommon (divisors, index)
import Data.Array
import System.IO

hasAny :: Eq a => [a] -> Bool
hasAny [] = False
hasAny xs = True

limit :: Int
limit = 20162

abundant :: Int -> Bool
abundant = (map abundant' [0..] !!)
  where abundant' n = (sum.divisors $ n) > n

abundantNumbers :: [Int]
abundantNumbers = filter (abundantArray !) [1..limit]
  where abundantArray = listArray (1, limit) $ map abundant [1..limit]

nonAbundant :: Int -> Bool
nonAbundant = (map nonAbundant' [0..] !!)
  where
    abundantDiffs n = filter (abundant) $ map (n-) $ takeWhile (<= half) abundantNumbers
      where half = n `div` 2
    nonAbundant' n = not $ hasAny (abundantDiffs n)

nonAbundantNumbers :: [Int]
nonAbundantNumbers = filter nonAbundant [1..limit]

problem_23 :: Integer
problem_23 = sum $ map toInteger nonAbundantNumbers

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStr $ "Memoizing abundant numbers... "
  putStrLn $ (show.length $ abundantNumbers) ++ " done!"

  putStr $ "Memoizing unabundant sum numbers... "
  putStrLn $ (show.length $ nonAbundantNumbers) ++ " done!"

  putStr $ "Calculating sum of unabundant numbers: "
  putStrLn $ show problem_23

