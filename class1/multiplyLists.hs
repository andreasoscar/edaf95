multiply :: Num a => [a] -> a
multiply [] = 1
multiply (x:xs) = x * (multiply xs)

main = print (multiply [1,2,3,4,5])