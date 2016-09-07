-- Finds 3 numbers that satisfy the equation a^2 + b^2 = c^2 exhasutively
-- In a more effiecent program it should be possible to just find a and b
-- Since C has to be c = 1000 - a - b
triplet n = take 1 [[a,b,c] | c<-[1..(n/2)], b<-[1..c], a<-[1..b], a^2 + b^2 == c^2, a + b + c == 1000]

main = do
    print (truncate (product(head (triplet 1000))))