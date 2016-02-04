{- What is the largest prime factor of the number 600851475143 -}

{- prime numbers taken from the haskell wiki -}
primes :: [Integer]
primes = 2: 3: sieve 0 (tail primes) 3
sieve k (p:ps) x = [n | n <- [x+2,x+4..p*p-2], and [n`rem`p/=0 | p <- fs]]
                   -- or:  all ((>0).(n`rem`)) fs
                   ++ sieve (k+1) ps (p*p)
                       where fs = take k (tail primes)

primeFactors :: Integer -> [Integer]
primeFactors x = [y | y <- primes, y*y <= x, x `mod` y == 0]
