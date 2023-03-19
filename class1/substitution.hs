substitute _ _ [] = []
substitute x y (z:zs)
    | z == x = y: (substitute x y zs)
    | otherwise = z: (substitute x y zs)


main = print (substitute 'e' 'i' "eigenvalue")