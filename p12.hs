{- What is the first triangular number to have more than 500 divisors
   A triangular number is a number formed by adding up the first N natural numbers
 -}

{-- A straightforward approach, far too slow --}
getTriangular :: Int -> Int
getTriangular n = sum [1..n]

countDivisors :: Int -> Int
countDivisors n = countHelp 0 1 n

countHelp :: Int -> Int -> Int -> Int
countHelp _ _ 0 = 0
countHelp accum index n =
    if index * 2 >= n + 1 then accum + 1
    else if mod n index == 0
         then countHelp (accum + 1) (index + 1) n
         else countHelp accum (index + 1) n

solve :: Int -> Int
solve start =
    if divisors >= 500
    then triangle
    else solve $ start + 1
    where triangle = getTriangular start
          divisors = countDivisors triangle

betterSolver :: Int -> Int -> (Int, Int)
betterSolver triangle step =
    if divisors >= 500
    then (triangle, divisors)
    else betterSolver (triangle + step)  $ step + 1
    where divisors = countDivisors triangle

displaySolver triangle step = do
  if divisors >= 500
  then return (triangle, divisors)
  else do
    putStrLn $ show step ++ " " ++ show triangle ++ " " ++ show divisors
    displaySolver (triangle + step) $ (step + 1)
    where divisors = countDivisors triangle

restart step =
    displaySolver triangle step
    where triangle = sum [1..step-1]


{-- A seive approach.  First find all numbers with >= 500 factors, then test for triangularity --}

isTriangular :: Integer -> Bool
isTriangular n = isTriangularHelp n 0 1
isTriangularHelp :: Integer -> Integer -> Integer -> Bool
isTriangularHelp target accum index =
    if accum > target
    then False
    else if accum == target
         then True
         else isTriangularHelp target (accum + index) (index + 1)

findManyFactors :: Integer -> Integer -> [Integer]
findManyFactors largestIndex factorThreshold =
    buildReturn factorThreshold indexes counts
    where indexes = [ 1..largestIndex ]
          accum = [ 0 | x <- indexes ]
          counts = foldl (findManyHelp indexes) accum indexes

findManyHelp :: [Integer] -> [Integer] -> Integer -> [Integer]
findManyHelp indexes accum current =
    map (mapfn current) pairs
    where pairs = zip accum indexes

mapfn :: Integer -> (Integer, Integer) -> Integer
mapfn current (s,i) =
    if mod i current == 0
    then s + 1
    else s

buildReturn :: Integer -> [Integer] -> [Integer] -> [Integer]
buildReturn _ [] [] = []
buildReturn _ [] _ = []
buildReturn _ _ [] = []
buildReturn thresh (hi:ti) (hc:tc) =
    if hc > thresh
    then hi : buildReturn thresh ti tc
    else buildReturn thresh ti tc

{-- see if the answer lies in range 1 .. upTo --}
attempt upTo =
    filter isTriangular $ findManyFactors upTo 500

{-- keep doubling the range until we find some solutions. --}
solve2 start =
    if res == []
    then solve2 $ start * 2
    else res
    where res = attempt start
