{- what is the millionth permutation of the digits 0..9 -}

chars = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

removeNth accum (h:t) 0 = (h, reverse accum ++ t)
removeNth accum (h:t) n = removeNth (h:accum) t (n - 1)

computePermutations accum [] = [reverse accum]
computePermutations accum lst =
    concatMap (\ (char, rem) -> computePermutations (char : accum) rem) nexts
    where cInd = [0 .. length lst - 1]
          nexts = map (removeNth [] lst) cInd

permutations = computePermutations [] chars
{- lol, 0-based indexing -}
ans = permutations !! (1000000 - 1)
