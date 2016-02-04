{- Sum of even valued fibonacci terms less than 4 million -}

{- standard recursive fib implementation -}
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib i = fib (i-1) + fib (i - 2)

{- Standard Dynamic Approach -}
dynFib :: Int -> Int
dynFib i = dynFibHelp i 1 0

dynFibHelp :: Int -> Int -> Int -> Int
dynFibHelp 0 iMinus1 iMinus2 = iMinus1
dynFibHelp i iMinus1 iMinus2 = dynFibHelp (i-1) (iMinus1 + iMinus2) iMinus1


dynFibList :: Int -> [Int]
dynFibList i = dynFibListHelp i 1 0

dynFibListHelp :: Int -> Int -> Int -> [Int]
dynFibListHelp 0 iMinus1 iMinus2 = [iMinus1]
dynFibListHelp i iMinus1 iMinus2 = iMinus1 : dynFibListHelp (i-1) (iMinus1 + iMinus2) iMinus1

{- Really, I should build the list so that I don't discard solutions later, but I'm lazy. -}
ans = sum [x | x <- dynFibList 50, x < 4000000, x `mod` 2 == 0]
