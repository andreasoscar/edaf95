  
module Sudoku where
    import Data.Char
    import Data.List
    import Data.Maybe hiding (fromMaybe)

    rows = "ABCD"
    cols = "1234"
    containsElem :: Eq a => a -> [a] -> Bool
    containsElem _ [] = False
    containsElem elem (x:xs)
        | elem == x = True
        | otherwise = containsElem elem xs

    
    cross :: [a] -> [a] -> [[a]]
    cross x [] = []
    cross [] y = []
    cross xs ys = [[x,y] | x <- xs, y <- ys] 

    replacePointsWithZeros :: [Char] -> [Char]
    replacePointsWithZeros x = map (\y -> if y == '.' then '0' else y) x

    crossBoard = cross rows cols

    parseBoard board = zip crossBoard (map (\y -> digitToInt y) (replacePointsWithZeros board))

    unitList :: [[String]]
    unitList = [cross rows [y] | y <- cols] ++ [cross [y] cols | y <- rows] ++ [cross x y | x <- ["AB", "CD"], y <- ["12", "34"]]

    filterUnitList :: String -> [[String]]
    filterUnitList x = filter (\y -> containsElem x y) unitList

    units :: String -> [(String, [[String]])]
    units x = [(x, filterUnitList x)]

    foldList :: [[a]] -> [a]
    foldList [] = []
    foldList (x:xs) = x ++ foldList xs

    removeDuplicates :: Eq a => [a] -> [a]
    removeDuplicates (x:xs) 
        | null xs = [x]
        | containsElem x xs = removeDuplicates xs
        | otherwise = [x] ++ removeDuplicates xs

    --peers :: [(String, [String])]
    --peers = map (\y -> (y, (delete y (removeDuplicates (foldList (filterUnitList y)))))) crossBoard
    peers :: [(String, [String])]
    peers = map (\x -> (fst x, [v | v <- removeDuplicates (foldList (snd x)), v /= (fst x)])) (foldList (map units crossBoard))

    fromMaybe :: a -> Maybe a -> a 
    fromMaybe a b 
        | isJust b = fromJust b 
        | otherwise = a

    getPeers :: String -> [String]
    getPeers a = fromMaybe [a] (lookup a peers)

    justifyList :: [Maybe a] -> [a]
    justifyList (x:xs)
        | null xs && isJust x = [fromJust x]
        | null xs = []
        | isJust x = [fromJust x] ++ justifyList xs 
        | otherwise = justifyList xs

    lookups :: Eq a => [a] -> [(a,b)] -> [b]
    lookups xs as = justifyList (map (\y -> lookup y as) xs)

    validSquare :: (String, Int) -> [(String, Int)] -> Bool 
    validSquare xs neighbors
        | snd xs == 0 = True 
        | elem (snd xs) (lookups (getPeers (fst xs)) neighbors) = False
        | otherwise = True
    

    validBoard :: [(String, Int)] -> Bool
    validBoard (x:xs)
        | null xs = True
        | validSquare x xs  = True && validBoard xs
        | otherwise = False

    verifySudoku :: String -> Bool 
    verifySudoku st = validBoard (parseBoard st)

    reduceList :: Eq a => [a] -> [a] -> [a]
    reduceList (x:xs) ys
        | null xs && elem x ys = []
        | null xs = [x]
        | elem x ys = [] ++ reduceList xs ys
        | otherwise = [x] ++ reduceList xs ys
        

    validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
    validSquareNumbers xs board = (fst xs, (snd xs) : reduceList [v | v <- map digitToInt cols] (lookups (getPeers (fst xs)) board))


    validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
    validBoardNumbers xs = map (\y -> validSquareNumbers y xs) xs

    validUnit :: [String] -> [(String, [Int])] -> Bool
    validUnit unit values = and [elem y (foldList (lookups unit values)) | y <- [1..4]] 


    validUnits :: [(String, Int)] -> Bool
    validUnits board = and [validUnit x (validBoardNumbers board) | x <- unitList]

    verifySudoku1 :: String -> Bool 
    verifySudoku1 st = validUnits (parseBoard st)

    chunks :: Int -> String -> [String]
    chunks n [] = []
    chunks n xs = take n xs : "\n" : chunks n (drop n xs)


    printSudoku :: String -> IO ()
    printSudoku [] = putStrLn ""
    printSudoku board = do
                            mapM_ putStr [x | x <- chunks 4 (concat
                                                                    [(
                                                                        if not (validSquare y (parseBoard board))
                                                                            then "X"
                                                                            else (show (snd y))
                                                                        ) | y <- (parseBoard board)])]
                            putStrLn ("===")
                            

                             

                            
                        
            
                    
                     
                    




