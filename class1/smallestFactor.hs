
nextFactor k n = minimum $ [x | x <- [1..n], n `mod` x == 0, x > k]

smallestFactor n = nextFactor 1 n

numFactors n = length $ [x | x <- [2..n], n `mod` x == 0]

main = print (numFactors 15)