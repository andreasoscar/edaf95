remove1 :: Eq a => a -> [a] -> [a]
remove1 x (y:ys) 
    | x == y = ys 
    | otherwise = y: (remove1 x ys)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True 
isPermutation [] _ = False 
isPermutation _ [] = False

isPermutation (x:xs) y
    | elem x y = isPermutation xs (remove1 x y)
    | otherwise = False

main = print (isPermutation [2,3,4,1,5] [3,1,2,5,4])