import Distribution.Simple.Utils (xargs)



shorter x xs = (length x) <= (minimum $ map length xs)
longer x xs = (length x) >= (maximum $ map length xs)
theLongest (x:xs)
    | null xs || longer x xs = x
    | otherwise = theLongest xs

theShortest (x:xs)
    | null xs || shorter x xs = x
    | otherwise = theShortest xs

shortestAndLongest a = (theShortest a, theLongest a)

main = print (shortestAndLongest ["hej", "pa", "dig", "idag", "huuuuu"])