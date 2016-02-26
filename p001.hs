{- Problem 1 : What's the sum of all numbers divisible by 3 or 5 less than 1000 -}

candidate :: Int -> Bool
candidate n =
    (n `mod` 5 == 0) || (n `mod` 3 == 0)

ans = sum [x | x <- [1..999], candidate x]
