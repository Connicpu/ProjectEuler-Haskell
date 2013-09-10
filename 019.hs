import Data.Time.Calendar (Day, fromGregorian, toGregorian, addDays)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import ConnorCommon (third)

firstSunday :: Integer -> Day
firstSunday year = fromGregorian year 1 firstSunday'
  where
    firstSunday' =
      if startDay == 0
        then 1
        else 8 - startDay
    startDay = snd . sundayStartWeek $ fromGregorian year 1 1

allSundays :: Integer -> [Day]
allSundays year = [ addDays (x * 7) (firstSunday year) | x <- [0..] ]

problem_19 :: Int
problem_19 = length [ sunday | sunday <- takeWhile (< endOfCentury) (allSundays 1901), isOnFirst sunday ]
  where
    isOnFirst day = (third $ toGregorian day) == 1
    endOfCentury = fromGregorian 2000 12 31

main :: IO ()
main = putStrLn $ "Sundays in the 20th century which fall on the first of the month: " ++ (show problem_19)
