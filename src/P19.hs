module P19 (
  p19 ) where

data WeekDay = Monday | Tuesday | Wednesday | Thursday |
               Friday | Saturday | Sunday
  deriving (Eq, Ord)

data Month = January Int | Feburary Int | March Int | April Int | May Int |
             June Int | July Int | August Int | September Int | October Int |
             November Int | December Int
  deriving Eq 

data Year = Year Int deriving Eq

data Date = Date WeekDay Month Year
  deriving Eq

bumpDay :: WeekDay -> WeekDay
bumpDay x | x < Tuesday = Tuesday
          | x < Wednesday = Wednesday
          | x < Thursday = Thursday
          | x < Friday = Friday
          | x < Saturday = Saturday
          | x < Sunday = Sunday
          | otherwise = Monday

bumpMonth :: Month -> Year -> Month
bumpMonth (January x) _   | x < 31 = January (x + 1)
                          | otherwise = Feburary 1
bumpMonth (March x) _     | x < 31 = March (x + 1)
                          | otherwise = April 1
bumpMonth (April x) _     | x < 30 = April (x + 1)
                          | otherwise = May 1
bumpMonth (May x) _       | x < 31 = May (x + 1)
                          | otherwise = June 1
bumpMonth (June x) _      | x < 30 = June (x + 1)
                          | otherwise = July 1
bumpMonth (July x) _      | x < 31 = July (x + 1)
                          | otherwise = August 1
bumpMonth (August x) _    | x < 31 = August (x + 1)
                          | otherwise = September 1
bumpMonth (September x) _ | x < 30 = September (x + 1)
                          | otherwise = October 1
bumpMonth (October x) _   | x < 31 = October (x + 1)
                          | otherwise = November 1
bumpMonth (November x) _  | x < 30 = November (x + 1)
                          | otherwise = December 1
bumpMonth (December x) _  | x < 31 = December (x + 1)
                          | otherwise = January 1
bumpMonth (Feburary x) (Year y)
  | x < 28 = Feburary (x + 1)
  | y `mod` 4 == 0 && y `mod` 100 /= 0 && x < 29 = Feburary (x + 1)
  | y `mod` 100 == 0 && y `mod` 400 == 0 && x < 29 = Feburary (x + 1)
  | otherwise = March 1

bumpYear :: Year -> Month -> Year
bumpYear (Year x) (December 31) = Year (x + 1)
bumpYear (Year x) _ = Year x

listDates :: Date -> [Date]
listDates (Date x y z) = Date x y z :
  listDates (Date (bumpDay x) (bumpMonth y z) (bumpYear z y))

testYear :: Date -> Bool
testYear (Date _ _ (Year x))
  | x <= 2000 = True
  | otherwise = False

testDay :: Date -> Bool
testDay (Date x y _) | x == Sunday && firstOfMonth y = True
                     | otherwise = False

firstOfMonth :: Month -> Bool
firstOfMonth x | x == January 1 = True
               | x == Feburary 1 = True
               | x == March 1 = True
               | x == April 1 = True
               | x == May 1 = True
               | x == June 1 = True
               | x == July 1 = True
               | x == August 1 = True
               | x == September 1 = True
               | x == October 1 = True
               | x == November 1 = True
               | x == December 1 = True
               | otherwise = False
  
testYear' :: Date -> Bool
testYear' (Date _ _ (Year x)) | x == 1900 = True
                              | otherwise = False

p19 :: Int
p19 = let dates = takeWhile testYear $
            listDates (Date Monday (January 1) (Year 1900))
          droppedDates = dropWhile testYear' dates
          filteredDates = filter testDay droppedDates
      in length filteredDates
