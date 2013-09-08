leastCommonMultiple :: Integer -> Integer -> Integer
leastCommonMultiple x y = head [ z | z <- [1..], mod z x == 0 && mod z y == 0 ]

leastCommonMultipleOfSet :: [Integer] -> Integer
leastCommonMultipleOfSet set = foldl leastCommonMultiple 1 set

main :: IO ()
main = print $ "LCM of [1..20]: " ++ (show (leastCommonMultipleOfSet [1..20]))
