getC :: Integer -> Integer -> Integer
getC a b = 1000 - a - b

isTriplet :: Integer -> Integer -> Bool
isTriplet a b = 
  a*a + b*b == c*c
  where c = getC a b

numRange :: [Integer]
numRange = [1..1000]

triplet :: (Integer, Integer, Integer)
triplet = head [
    (a, b, getC a b) |
    a <- numRange,
    b <- numRange,
    isTriplet a b
  ]

main :: IO ()
main = putStrLn $ "Product of pythagorean triplet triangle with perimeter of 1000: " ++ (show triplet)
