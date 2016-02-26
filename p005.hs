{- What is the smallest positive number evenly divisible by all of the numbers from 1..20 -}

divisibleBy :: [Integer] -> Integer -> Bool
divisibleBy [] _ = True
divisibleBy (hd:tl) n = n `mod` hd == 0 && divisibleBy tl n

findSmallest :: (Integer -> Bool) -> Integer -> Integer
findSmallest predicate stepSize =
    findSmallestHelp predicate stepSize stepSize
findSmallestHelp predicate stepSize current =
    if predicate current
    then current
    else findSmallestHelp predicate stepSize (current + stepSize)

{- reverse that list because I want the trial division to fail fast -}
divisibleUpTo20 = divisibleBy $ reverse [1..20]

{- for this problem, the step size is probably something like the largest LCM of all pairs 1..20
   so, 20 is conservative, but you still find the answer in seconds, so whatever. -}
ans = findSmallest divisibleUpTo20 20
