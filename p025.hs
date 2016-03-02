{- What is the first numberin the fibonnacci sequence to contain more than 1000 digits -}

fib 1 = 1
fib 2 = 1
fib n = fibDP (n - 3) 1 1

fibDP 0 nm1 nm2 =
    nm1 + nm2

fibDP target nm1 nm2 =
    fibDP (target - 1) nm1' nm2'
    where next = nm1 + nm2
          nm1' = next
          nm2' = nm1

customFibDP target index nm1 nm2 =
    if (length (show next)) >= target
    then index
    else customFibDP target (index + 1) next nm1
    where next = nm1 + nm2

ans = customFibDP 1000 3 1 1
check = length (show (fib ans)) == 1000
