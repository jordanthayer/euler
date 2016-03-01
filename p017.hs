{- How many characters are used when writing out the numbers 1 to 1000 inclusive, ignoring space and - -}

digit 0 = "zero"
digit 1 = "one"
digit 2 = "two"
digit 3 = "three"
digit 4 = "four"
digit 5 = "five"
digit 6 = "six"
digit 7 = "seven"
digit 8 = "eight"
digit 9 = "nine"

tens 10 = "ten"
tens 11 = "eleven"
tens 12 = "twelve"
tens 13 = "thirteen"
tens 14 = "fourteen"
tens 15 = "fifteen"
tens 16 = "sixteen"
tens 17 = "seventeen"
tens 18 = "eighteen"
tens 19 = "nineteen"
tens 20 = "twenty"
tens 30 = "thirty"
tens 40 = "forty"
tens 50 = "fifty"
tens 60 = "sixty"
tens 70 = "seventy"
tens 80 = "eighty"
tens 90 = "ninety"
tens x
 | x > 20 && x < 30 = tens 20 ++ '-' : (digit $ x - 20)
 | x > 30 && x < 40 = tens 30 ++ '-' : (digit $ x - 30)
 | x > 40 && x < 50 = tens 40 ++ '-' : (digit $ x - 40)
 | x > 50 && x < 60 = tens 50 ++ '-' : (digit $ x - 50)
 | x > 60 && x < 70 = tens 60 ++ '-' : (digit $ x - 60)
 | x > 70 && x < 80 = tens 70 ++ '-' : (digit $ x - 70)
 | x > 80 && x < 90 = tens 80 ++ '-' : (digit $ x - 80)
 | x > 90 && x < 100 = tens 90 ++ '-' : (digit $ x - 90)

hundreds x =
    if x == hplace * 100
    then base
    else if rem == 0
         then base
         else base ++ " and " ++ (numberString rem)
    where hplace = quot x 100
          base = digit hplace ++ " hundred"
          rem = mod x 100

thousand x = "one thousand"

numberString n
 | n < 0 = ""
 | n < 10 = digit n
 | n >= 10 && n < 100 = tens n
 | n >= 100 && n < 1000 = hundreds n
 | n >= 1000 = thousand n

counted c
    | c == ' ' = False
    | c == '-' = False
    | True = True

relevantNums = [1..1000]
asPrettyStrings = map numberString relevantNums
asCountStrings = map (filter counted) asPrettyStrings
asLengths = map length asCountStrings
ans = sum asLengths

