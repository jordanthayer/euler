{- A palindromic number reads the same both ways.  What is the largest palindrome made from the product of two 3-digit numbers? -}

threeDigits = [100..999]
threeDigitProducts = [ x * y | x <- threeDigits, y <- threeDigits, x > y] ++ [x*x | x <- threeDigits ]

palindromicNumber :: Integer -> Bool
palindromicNumber n =
    isPalindrome $ show n

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

palindromicProducts = [ x | x <- threeDigitProducts, palindromicNumber x ]
ans = maximum palindromicProducts
