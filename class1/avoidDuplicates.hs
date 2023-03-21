duplicates :: Eq a => [a] -> Bool 

duplicates [] = False
duplicates (x:xs) 
    | elem x xs = True 
    | otherwise = (duplicates xs)

main = print (duplicates [1,2,3,2])

