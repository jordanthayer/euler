import Data.List
{-

Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n).  If d(a) = b and d(b) = a, where a ≠ b, then a and
b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

-}


getDivisors :: Int -> [Int]
getDivisors n = helper 1 n

helper :: Int -> Int -> [Int]
helper _ 0 = []
helper index n =
    if index * 2 >= n + 1 then []
    else if mod n index == 0
         then index : (helper (index + 1) n)
         else helper (index + 1) n


d :: Int -> Int
d n = sum $ getDivisors n


findPairs 0 = []
findPairs 1 = []
findPairs n =
    if amicable
    then (n, candidate) : findPairs (n - 1)
    else findPairs (n - 1)
    where candidate = d n
          reciprocal = d candidate
          amicable = reciprocal == n && not (n == candidate)

flattenTuples [] = []
flattenTuples ((a,b):tl) = a : b : flattenTuples tl

uniqueify [] = []
uniqueify [x] = [x]
uniqueify (a:b:tl) =
    if a == b
    then uniqueify (b:tl)
    else a : uniqueify (b : tl)

ans = sum $ uniqueify $ sort $ flattenTuples $ findPairs 10000
