sumsqrec 0 = 0
sumsqrec n = n*n + sumsqrec (n-1)

sumsqmap n = map (\x -> x*x) [x | x <- [1..5]]
main = print (sumsqrec 5)