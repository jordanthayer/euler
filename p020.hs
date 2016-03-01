{- digit sum of 100! -}

asString number = show number
asDigitList str = [ read (x:[]) :: Integer | x <- str ]

fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)

number = fact 100
string = asString number
digits = asDigitList string
ans = sum digits
