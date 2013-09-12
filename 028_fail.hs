type Grid = [[Int]]

spiralGrid :: Int -> Grid
spiralGrid n = 
  spiralGrid' ((n `div` 2) + 1)
  where
    spiralGrid' 1 = [[1]]
    spiralGrid' n =
      [ [ numAt n x y | x <- [0..dm1] ] | y <- [0..dm1] ]
      where
        diameter = n * 2 - 1
        dm1 = diameter - 1
        numAt 1 _ _ = 1
        numAt n x y =
          numAt' x y
          where
            numAt' x y
              | y == 0    = top !! x
              | y == dm1  = bottom !! x
              | x == 0    = left !! (y - 1)
              | x == dm1  = right !! (y - 1)
              | True      = numAt (n - 1) (x - 1) (y - 1)

            start = innerDiameter^2 + 1
            innerDiameter = (n - 1) * 2 - 1

            diameter = n * 2 - 1
            dm1 = diameter - 1

            right = [start .. (start + dm1 - 2)]
            bottom = reverse [(start + dm1 - 1) .. (start + 2*dm1 - 1)]
            left = reverse [(start + 2*dm1) .. (start + 3*dm1 - 2)]
            top = [(start + 3*dm1 - 1) .. (start + 4*dm1 - 1)]

printGrid :: Grid -> String
printGrid grid = foldl (\s r -> s ++ (foldl (\s i -> s ++ (sdig i) ++ " ") [] r) ++ ['\n' | x <- [1..(digl `div` 2) + 1]]) "" grid
  where
    digl = length.show.head.head $ grid
    sdig d = [ ' ' | x <- [n..digl-1] ] ++ (show d)
      where n = length.show $ d

lDiag :: Grid -> [Integer]
lDiag grid = [ toInteger.(!!n).(!!n) $ grid | n <- [0..(length grid) - 1] ]
rDiag :: Grid -> [Integer]
rDiag grid = [ toInteger.(!!n).(!!((length grid)-1-n)) $ grid | n <- [0..(length grid) - 1] ]

