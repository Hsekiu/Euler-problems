-- Neat little indentify makes sumsquares easily and fast to solve
sumsquare n = fromIntegral (n*(n+1)*(2*n+1)) `div` 6

squaresum n = (sum [1..n])^2

main = print ((sumsquare 100) - (squaresum 100))