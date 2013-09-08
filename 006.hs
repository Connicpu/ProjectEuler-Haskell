squares :: (Num a) => [a] -> [a]
squares set = [ x*x | x <- set ]

square :: (Num a) => a -> a
square x = x*x

numbers = [1..100]

main :: IO ()
main = print $ "Difference between sum of squares and square of sum: " ++ ((show.abs) $ ((sum.squares) numbers) - ((square.sum) numbers))
