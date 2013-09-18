import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.List.Ordered (sort)
import ConnorCommon (index)

charValue :: Char -> Integer
charValue char = (index (toLower char) ['a'..'z']) + 1

names :: String -> [String]
names raw = [ read n :: String | n <- splitOn "," raw ]

sortedNames :: [String] -> [String]
sortedNames xs = sort xs

nameValue :: String -> [String] -> Integer
nameValue name list = (1 + index name list) * (sum [ charValue x | x <- name ])

problem_22 :: String -> Integer
problem_22 nameData =
  sum [ nameValue name nameList | name <- nameList ]
  where nameList = sortedNames.names $ nameData

main :: IO ()
main = do
  nameData <- readFile "022_names.txt"
  putStrLn $ "Value of all names in the list: " ++ (show . problem_22 $ nameData)
