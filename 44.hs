{-# language BangPatterns #-}
{-# language TypeApplications #-}
import qualified Data.List.Ordered as OList

main = print answer

answer = head $ filter predicate $ sp pentagonals

predicate (a,b,c,d) = c - b == a && b + c == d

pentagonal n = n * (3 * n - 1) `div` 2
pentagonals = scanl1 (+) [1,4..]

-- | given an ordered list A = [a_i], produce the list
-- of lists of values [l_i] st [x in l_i <-> x > a_i, there may be
--                               an a_j in A with a_j - x = a_i]       
sp xs = moving xs xs xs xs

{-# noinline moving #-}
moving (b:bs) (c:cs) (d:ds) (e:es) 
            | d >= e    = moving (b:bs) (c:cs) (d:ds)    es
            | c >= d    = moving (b:bs) (c:cs)    ds  (e:es)
            | e < c + d = moving (b:bs) (c:cs) (d:ds)    es
            | b >= c    = moving (b:bs)    cs  (d:ds) (e:es)
            | b < d - c = moving    bs  (c:cs) (d:ds) (e:es)  
            | b > d - c = moving (b:bs)    cs  (d:ds) (e:es)  
            | otherwise = (b,c,d,e) : moving bs  (c:cs) (d:ds) (e:es)  