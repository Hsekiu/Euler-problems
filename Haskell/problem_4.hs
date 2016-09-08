-- This likely doesn't need a palindrome function or be so many lines
-- But a palindrome function is nessasiry for problem 6 so I will re-write
-- at a future date, it's also very slow.

import Data.List

toList :: Integral x => x -> [x]
toList 0 = []
toList x = toList (x `div` 10) ++ [x `mod` 10]

toDigit x = foldl add 0 x where add x y = 10*x + y

isPal :: (Eq a) => [a] -> Bool
isPal [] = True
isPal (x:[]) = True
isPal (x:y:[]) = if x == y then True else False
isPal (x:xs) = (x == (last xs)) && (isPal (init(xs)))

genCan :: (Enum x, Integral y, Num x) => [x] -> y -> [x]
genCan [] y = []
genCan (x:xs) y = map (*x) [10^(y-1)..(10^y-1)] ++ genCan (xs) y

genCan2 x = genCan [10^(x-1)..(10^x-1)] x

largePal x = last (sort (map toDigit (filter isPal (map toList (genCan2 x)))))

main = print (largePal 3)