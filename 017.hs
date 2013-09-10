import Data.Char

one = [
    "one","two","three","four","five","six","seven","eight",
    "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
    "sixteen","seventeen","eighteen", "nineteen"
  ]
ty = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

decompose :: Int -> String
decompose x
  | x == 0 = []
  | x < 20 = 
    one !! (x - 1)
  | x >= 20 && x < 100 = 
    ty !! (firstDigit (x) - 2) ++ decompose (x - firstDigit (x) * 10)
  | x < 1000 && x `mod` 100 == 0 = 
    one !! (firstDigit (x) - 1) ++ "hundred"
  | x > 100 && x <= 999 =
    one !! (firstDigit (x) - 1) ++ "hundredand" ++ decompose (x - firstDigit (x) * 100)
  | x == 1000 = "onethousand"
  where firstDigit x = digitToInt.head.show $ x

problem_17 :: Integer
problem_17 = sum [ fromIntegral.length.decompose $ x | x <- [1..1000] ]

main :: IO ()
main = putStrLn $ "Sum of decomposition of all numbers from 1 to 1000: " ++ (show problem_17)
