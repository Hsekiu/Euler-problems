primes :: [Int]
primes = 2 : 3 : filter isPrime [5, 7..]

isPrime :: Integral x => x -> Bool
isPrime x = if (length(fact x) == 1) then True else False

fact n = [i | i <-[1..k], (n `mod` i) == 0] 
   where k = ceiling (sqrt (fromIntegral  n))

--extremely slow prime generator is naive and should be replaced
main = do
    print (sum (takeWhile (< 2000000) primes))