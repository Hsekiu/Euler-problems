mult_sum n m i = sum[x | x <- [1..(n-1)], x `mod` m == 0 || x `mod` i == 0] 

main = print (mult_sum 1000 3 5)