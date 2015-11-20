--List comprehension to find list of factors that are less then
--or equal to sqrt(x) of x, since prime factors cannot be larger
--then this set.
fact n = [i | i <-[1..k], (n `mod` i) == 0] 
   where k = ceiling (sqrt (fromIntegral  n))

isPrime :: Integral x => x -> Bool
isPrime x = if (length(fact x) == 1) then True else False

--Filter the small factor list for prime numbers and return largest
largeFactPrime x = last (filter isPrime (fact x))