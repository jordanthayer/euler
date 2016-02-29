{- An equivalent question is how many binary strings are there of length 2*N containing N 1s -}

fact 0 = 1
fact 1 = 1
fact n = n * (fact $ n - 1)

binomaial n k = quot (fact n) ((fact k) * (fact $ n - k))

ans = binomaial 40 20


