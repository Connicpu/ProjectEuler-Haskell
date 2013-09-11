import Data.List (permutations)
import Data.List.Ordered (sort)

problem_24 :: String
problem_24 = (sort $ permutations "0123456789") !! (999999)

main :: IO ()
main = putStrLn $ "1000000th permutation of 0123456789 in lexographicalOrder: " ++ (show problem_24)
