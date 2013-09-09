-- Woo, combinations :D
waysToTraverseGrid :: Integer -> Integer
waysToTraverseGrid size = product [size+1..size+size] `div` product [2..size]

main :: IO ()
main = putStrLn $ "Routes through a 20x20 grid: " ++ (show $ waysToTraverseGrid 20)
