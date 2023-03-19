sumsq n = sum $ zipWith (*) [1..n] [1..n]

main = print (sumsq 5)