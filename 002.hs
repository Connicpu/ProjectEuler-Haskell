import System.Environment

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

isEven x = mod x 2 == 0

appendEvenFibonaccy :: Integer -> [Integer] -> [Integer]
appendEvenFibonaccy fx list =
  if isEven fx
    then fx:list
    else list

evenFibonacci :: Integer -> Integer -> [Integer] -> [Integer]
evenFibonacci x max list =
  if fx >= max 
    then list 
    else evenFibonacci (x+1) max (appendEvenFibonaccy fx list)
  where fx = fibonacci x

evenFibonacciSum :: Integer -> [Char]
evenFibonacciSum max = show (sum (evenFibonacci 0 max []))

main :: IO ()
main = do print ("Sum: " ++ (evenFibonacciSum 4000000))
