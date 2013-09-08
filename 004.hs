import Data.List

isPalindrome :: [Char] -> Bool
isPalindrome string =
  if length string < 2
    then True
    else
      xs == xl && isPalindrome chopped
      where
        xs = head string
        xl = last string
        chopped = (tail . init) string

all3DigitNumbers :: [Integer]
all3DigitNumbers = [100..999]

palindromeProductsOfThreeDigits :: [Integer]
palindromeProductsOfThreeDigits = [ x | x <- [ x * y | x <- all3DigitNumbers, y <- all3DigitNumbers ], isPalindrome (show x)]

sortedPalindromes :: [Integer]
sortedPalindromes = sort palindromeProductsOfThreeDigits

largestPalindrome :: Integer
largestPalindrome = last sortedPalindromes

main :: IO ()
main = print $ "Largest: " ++ (show largestPalindrome)
