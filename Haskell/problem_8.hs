import Data.List.Split

-- Takes a list of ints and finds sucessive products of consecutive
--sublists of 13 elements
findProd :: [Int] -> [Int]
findProd [] = []
findProd (x:xs) = 
    if ((length xs) > 2) 
        then [x * (product (take 12 xs))] ++ (findProd xs)
    else []

-- Input is large so read it from a file and create a list of ints
main = do
    contents <- readFile "input_8.txt"
    print (maximum (findProd (map (read . (:"")) contents :: [Int])))