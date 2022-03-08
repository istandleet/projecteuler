module Main where

main = print $ answer 2022

_appmod = flip mod 1000000007

answer bigN = _appmod $ sum $ map go [1..bigN]
    where
    go n = _appmod
         $ 9 * d (min bigN (2*n-1)) n 1
             + d (min bigN (2*n-1)) n 0

d bigN n _ | n <= bigN `div` 2 = 0
d bigN n 0 = (9*) $ sum $ map (\digits -> d digits n 1) [n..bigN-1]
d bigN n b
    = (bigN `choose` n) * 9 ^ (bigN-n)


choose :: Integer -> Integer -> Integer
n `choose` k = product [1+max k (n-k)..n] `div` product [1..min k (n-k)]
