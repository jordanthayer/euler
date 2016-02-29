{- What is the sum of the digits of the number 2^1000 -}
convert :: String -> Integer
convert x = read x

number = 2^1000
asString = show number
asDigits = map (\ x -> [x]) asString
asInts = map convert asDigits
solution = sum asInts
