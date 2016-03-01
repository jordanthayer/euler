{- How many Sundays Fell on the first of the month in the 20th century (1 Jan 1901 to 31 Dec 2000) -}

data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday deriving (Show, Eq)

next :: Day -> Day
next Sunday = Monday
next Monday = Tuesday
next Tuesday = Wednesday
next Wednesday = Thursday
next Thursday = Friday
next Friday = Saturday
next Saturday = Sunday

isLeapYear :: Integer -> Bool
isLeapYear x =
    (four && (not hundred)) || (four && fourHundred)
    where four = mod x 4 == 0
          hundred = mod x 100 == 0
          fourHundred = mod x 400 == 0

daysPerYear :: Integer -> Integer
daysPerYear yr =
    if isLeapYear yr
    then 366
    else 365

dayOfMonths leapYear =
    jan ++ feb ++ march ++ april ++ may ++ june ++ july ++ august ++ september ++ october ++ november ++ december
    where jan = [1..31]
          feb = if isLeapYear leapYear then [1..29] else [1..28]
          march = [1..31]
          april = [1..30]
          may = [1..31]
          june  = [1..30]
          july = [1..31]
          august = [1..31]
          september = [1..30]
          october = [1..31]
          november = [1..30]
          december = [1..31]

daysInYears :: [Integer] -> Integer
daysInYears [] = 0
daysInYears (hd:tl) = daysPerYear hd + daysInYears tl

dayList :: Day -> Integer -> [Day]
dayList _ 0 = []
dayList day n =
    day : dayList (next day) (n - 1)

years = [1901..2000]
dayOfWeek = dayList Tuesday (daysInYears years)
dayOfMonth = concat $ map dayOfMonths years
dates = zip dayOfMonth dayOfWeek
firstOfMonthMondays = filter (\ (dom, dow) -> dom == 1 && dow == Sunday) dates
ans = length firstOfMonthMondays
