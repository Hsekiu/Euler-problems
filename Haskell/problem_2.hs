-- Simple and very slow O(n^2) should change for something that runs
-- at O(log n) like tail recursion or matrix exponentiation
fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | n >= 2 = fib (n-1) + fib (n-2)

-- Using haskell lazy evalution and filtering out wanted numbers
fibEvenSum n = sum(filter even (takeWhile (< n) (map fib [2..])))

main = print (fibEvenSum 4000000)
