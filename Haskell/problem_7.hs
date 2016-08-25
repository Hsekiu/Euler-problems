-- A servicable lazy prime list generator
primes :: [Int]
primes = 2 : 3 : filter isPrime [5, 7..]

fact n = [i | i <-[1..k], (n `mod` i) == 0] 
   where k = ceiling (sqrt (fromIntegral  n))

isPrime :: Integral x => x -> Bool
isPrime x = if (length(fact x) == 1) then True else False

main = print (primes !! 10000)