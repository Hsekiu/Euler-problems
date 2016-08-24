is_div n = [x | x <- [1..20], mod n x == 0]

-- Slow solution that finds the first number that is divisible by the range
-- 1..20 looks through numbers that are multiple of 20's and could be made 
-- by cutting down check range (if divisible by 20 is divisible by 2) or 
-- just applying prime factorization.
main = print (take 1 [x | x <- [20,40..], is_div x == [1..20]])

-- A fast solution that goes over the multiples and their products and find the lcm
fast = foldl1 lcm [1..20]