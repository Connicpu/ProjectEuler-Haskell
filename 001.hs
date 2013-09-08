import System.Environment

isMultipleOf3 x = mod x 3 == 0
isMultipleOf5 x = mod x 5 == 0

listOfMultiples :: Integer -> [Integer] -> [Integer]
listOfMultiples 0 list = list
listOfMultiples x list = listOfMultiples (x - 1) (if isMultipleOf3 x || isMultipleOf5 x then x:list else list)

sumOfMultiples :: Integer -> [Char]
sumOfMultiples max = show (sum (listOfMultiples max []))

main :: IO ()
main = do print ("Sum: " ++ (sumOfMultiples 999))
