{- what is the sum of all prime numbers less than 2_000_000? -}

{- prime numbers taken from the haskell wiki -}
primes :: [Integer]
primes = 2: 3: sieve 0 (tail primes) 3
sieve k (p:ps) x = [n | n <- [x+2,x+4..p*p-2], and [n`rem`p/=0 | p <- fs]]
                   -- or:  all ((>0).(n`rem`)) fs
                   ++ sieve (k+1) ps (p*p)
                       where fs = take k (tail primes)

prefix target [] = []
prefix target (hd:tl) =
    if hd > target then []
    else hd : prefix target tl

ans = sum $ prefix 2000000 primes
