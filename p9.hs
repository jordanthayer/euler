{- Find the pythagorean tripple who's sum is 1000
   a^2 + b^2 = c^2, a < b < c -}

direct = [(a,b,c) | c <- [3..1000], b <- [2..c], a <- [1..b],
                         a < b && b < c && a^2 + b^2 == c^2 && a + b + c == 1000]
