type Month = Integer 

daysInMonth :: Month -> Integer -> Integer 
daysInMonth m year
    | m <= 0 || m > 12 = 0
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12   = 31
    | m == 2 && year `mod` 4 == 0 = 29
    | m == 2 = 28
    | m == 4 || m == 6 || m == 9 || m == 11 = 30

data Date = Date Integer Month Integer
validDate :: Date -> Bool 
validDate (Date y m d)
    | d > 0 && d < (daysInMonth m y) = True
    | otherwise = False



main = print (validDate (Date 2020 2 2))