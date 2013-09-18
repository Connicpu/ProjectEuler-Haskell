type Triangle = [[Integer]]

triangle :: String -> Triangle
triangle str = [ [ read x | x <- words line ] | line <- lines str ]

highestValueRoute :: Triangle -> Integer
highestValueRoute tri = head $ routeValues tri

routeValues :: Triangle -> [Integer] 
routeValues tri = foldr1 routeValue tri
  where
    localTriValue x y z = x + max y z
    routeValue xs ys = zipWith3 localTriValue xs ys $ tail ys

main :: IO ()
main = do
  triangleData <- readFile "067_triangle.txt"
  putStrLn $ "Triangle: " ++ (show . highestValueRoute $ triangle triangleData)
