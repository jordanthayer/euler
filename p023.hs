{- Find the sum of all the positive ingeres which cannot be written as the sum of two abundant numbers.
   These numbers must lie in the range 0 .. 28123 -}

import Data.List

data Kind = Perfect | Deficient | Abundant deriving (Eq, Show)

getDivisors :: Int -> [Int]
getDivisors n = helper 1 n

helper :: Int -> Int -> [Int]
helper _ 0 = []
helper index n =
    if index * 2 >= n + 1 then []
    else if mod n index == 0
         then index : (helper (index + 1) n)
         else helper (index + 1) n

classify n =
    if n == divSum
    then Perfect
    else if n > divSum
         then Deficient
         else Abundant
    where divSum = sum $ getDivisors n


candidates = [12..28123]
abundants = filter (\x -> classify x == Abundant) candidates

{- now, build the list of all pairwise sums -}
sumPairs [] = []
{- single ton needs to be added to itself -}
sumPairs [hd] = [hd + hd]
{- if there is more than one item in the list, add it to itself and map an add of it to every other item. -}
sumPairs (a:b:tl)  =
    a + a : map (a +) (b:tl) ++ sumPairs (b:tl)

{- reduce a sorted list to its unique elements -}
uniqueify [] = []
uniqueify [x] = [x]
uniqueify (a:b:tl) =
    if a == b
    then uniqueify (b:tl)
    else a : uniqueify (b : tl)

{- give me a list of all unique sums of two abundant numbers -}
sumTwoAbundants = uniqueify $ sort $ sumPairs abundants

{- given an ordered list of numbers, give me a list of numbers not in that list. -}
notInList _ [] = []
notInList current (hd:tl) =
    if current == hd
    then notInList (current + 1) tl
    else if current > hd
         then notInList current tl
         else [current..hd-1] ++ notInList (hd + 1) tl

candidateNotAbundants = [1..28123]

notInListPrime [] _ = []
notInListPrime l [] = l
notInListPrime (a:ta) (b:tb) =
    if a == b
    then notInListPrime ta tb
    else a : notInListPrime ta rest
    where rest = if a > b then tb else b : tb

notSumAbundants = notInListPrime candidateNotAbundants sumTwoAbundants
ans = sum notSumAbundants
