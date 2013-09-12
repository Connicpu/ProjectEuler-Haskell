type Grid = [[Int]]

ringCornerSum :: Integer -> Integer
ringCornerSum n = ringCornerSum' n
  where
    ringCornerSum' 1 = 1
    ringCornerSum' n = tl + tr + bl + br + (ringCornerSum' $ n - 2)
      where
        tr = n^2
        tl = tr - (n - 1)
        bl = tl - (n - 1)
        br = bl - (n - 1)

problem_27 :: Integer
problem_27 = ringCornerSum 1001

main :: IO ()
main = putStrLn $ "Sum of diagonals for 1001x1001 grid: " ++ (show problem_27)
