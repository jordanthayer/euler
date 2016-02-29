next n =
    if (mod n 2) == 0
    then quot n 2
    else 3 * n + 1

series n =
    if n == 1
    then [n]
    else n : (series $ next n)

count n =
    if n == 1
    then 1
    else 1 + (count $ next n)

{- I should memoize this, but probably don't need to since the input is so small -}

solve stop = solveHelp stop 1 1 1

solveHelp stop bestCount bestIndex curIndex =
    if curIndex > stop
    then (bestCount, bestIndex)
    else if curCount > bestCount
         then solveHelp stop curCount curIndex index'
         else solveHelp stop bestCount bestIndex index'
    where index' = curIndex + 1
          curCount = count curIndex
