{- Find the difference between the sum of the squarse of the first 100 natural numbers and the square of that sum -}

first100 = [1..100]
sumFirst = sum first100

ans = sumFirst * sumFirst - sum [ x * x | x <- first100 ]
