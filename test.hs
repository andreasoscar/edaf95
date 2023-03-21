import Foreign (Storable(alignment))
import Language.Haskell.TH (promotedTupleT)
-- Have (.)(:)

-- 1. (.) operator is composition, has type (b -> c) -> (a -> b) -> a -> c
--  (b -> c           ) -> (a -> b) -> a -> c
--   a -> [a] -> [a]
-- b equivalent to a, and c equivalent to [a] -> [a]
-- remainder is (a -> b) -> a -> c, replace c with [a] -> [a] and with b, it becomes c = [b] -> [b] and flipped before inserting c is
-- (b -> a) -> b -> [a] -> [a] 


-- (.)(.), has type (b -> c) -> (a -> b) -> a -> c

-- first has (b -> c                         ) -> (a -> b) -> a -> c
-- second has (d -> e) -> ((f -> d) -> f -> e)

-- Know that b equivalent to d->e  and c quivalent with (f -> d) -> f -> e, the remainder is (a -> b) -> a -> c
-- becomes (a -> d -> e) -> a -> (f -> d) -> f -> e

-- (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c

-- (.)(.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (b -> c                             ) -> (a -> b) -> a -> c
-- (b -> c) -> (a -> b) -> a -> c
-- b -> c equivalent with b, (a -> b) -> a -> c equivalent with c
-- (a -> b) -> a -> c
-- (a -> b -> c) -> a -> (a -> b) -> a -> c
-- a1 -> b1 -> c -> a1 -> (a1 -> b1) -> a1 -> c
-- (a1 -> b1 -> c) -> a1 -> (a1 -> b1) -> a1 -> c


-- Have (.)(:)

-- 1. (.) operator is composition, has type (b -> c) -> (a -> b) -> a -> c
--  (b -> c         ) -> (a -> b) -> a -> c
--   a -> [a] -> [a]
-- a equivalent to b, and [a] -> [a] equivalent to c
-- (a -> b) -> a -> c
--  b    a     b    [a] -> [a]
-- b1    a1    b1    a1     a1 
-- (b1 -> a1) -> b1 -> [a1] -> [a1]

-- (b -> c) -> (a -> b) -> a -> c

-- (+) (:)
-- (:) :: a -> [a] -> [a]
-- (+) :: a -> a -> a

-- (a -> a          ) -> a
-- (a1 -> [a1] -> [a1]
-- a equivalent to a1, a equivalent to [a1] -> [a1]
-- rest is a 
-- a
-- [a1] -> [a1]
-- 

mystery xs = foldr (++) [] (map (\y -> [y]) xs)


main = print (mystery [1,2,3,4])
-- (map (\y -> [y]) xs) takes every element in xs and makes it into a list,
-- so if xs was [1,2,3] before, after map it becomes [[1], [2], [3]]
-- foldr works from the right side, and (++) with [] generates the list itself
-- result is [1,2,3] because it does ([1] ++ ([2] ++ ([3] ++ ([]))))