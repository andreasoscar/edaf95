sumsqrec 0 = 0
sumsqrec n = n*n + sumsqrec (n-1)

sumsqmap n = sum $ map (\x -> x*x) [x | x <- [1..5]]

sumsqalternative n = sum $ zipWith (*) [1..n] [1..n]

main = print (sumsqmap 5)