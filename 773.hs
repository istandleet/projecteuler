{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.List
import qualified Data.Set as Set
import qualified Math.NumberTheory.Primes as P

-- f 5000 = 929701790 in 1.7s 
main :: IO ()
main = interact $ show . mo . fastAnswer . read

mo :: Integer -> Integer
mo = flip mod 1000000007

fastAnswer :: Int -> Integer
fastAnswer k = pn * (5*(fastPPart !! k) + fastQPart k)
    where
    p7s = take k $ filter (\p -> p `mod` 10 == 7) $ map P.unPrime P.primes
    pn = product p7s

fastQPart :: Int -> Integer
fastQPart k = sum (map go [0..k])
    where
    go :: Int -> Integer
    go l = (*((-1)^l))
         $ (q l-5) * (fromIntegral k `choose` fromIntegral l)
    q l = case l `mod` 4 of 
        0 -> 7 -- xx1
        1 -> 1 -- xx7
        2 -> 3 -- xx9
        3 -> 9 -- xx3

fastPPart :: [Integer]
fastPPart = scanl1 go $ (1:) $ filter (\p -> p `mod` 10 == 7) $ map P.unPrime P.primes
    where go f p = mo $ (p-1) * f

-- * showing work
answer :: Int -> Integer
answer k = 
    let ss = s k
        n = product ss
     in sum $ filter (\i -> not $ any (`divides` i) ss) [7,17..n]

s :: Int -> [Integer]
s k = 2 : 5 : take k (filter (\p -> p `mod` 10 == 7) primes)

answer' :: Int -> Integer
answer' k = linearSumLT 10 7 n - sum nons
  where
    ss = s k
    sevens = drop 2 ss
    n = product ss
    nons = foldMap go sevens
    go p = Set.fromDistinctAscList $ takeWhile (<n) $ map (*p) [1,11..]

answer3 = linearSumLT 10 7 n
        - go 7 - go 17 - go 37
        + go (7*17) + go (7*37) + go (17*37)
        - go (7*17*37)
    where
    go p = let q = head $ filter (\i -> mod i 10 == 7) $ iterate (+p) p
            in linearSumLT (10*p) q n
    n = product $ s 3

answer'' :: Int -> Integer
answer'' k = linearSumLT 10 7 n + sum (map gogo [1..k])
    where
    gogo :: Int -> Integer
    gogo i = (if odd i then negate else id) 
           $ sum 
           $ map ((\p -> linearSumLT (10*p) (offset i*p) n) . product) 
           $ choosing i p7s
    p7s = take k (filter (\p -> p `mod` 10 == 7) primes)
    n = 10 * product p7s
    offset l = case l `mod` 4 of 
        0 -> 7 -- xx1
        1 -> 1 -- xx7
        2 -> 3 -- xx9
        3 -> 9 -- xx3

-- | Current solution. same as above, just did some algebra/inlining
answer''' :: Int -> Integer
answer''' k = pn * sum (map gogo [0..k])
    where
    gogo :: Int -> Integer
    gogo l = (*((-1)^l))
           $ 5 * sum (map (div pn . product) $ choosing l p7s)
           + (q l-5) * (fromIntegral k `choose` fromIntegral l)
    p7s = take k (filter (\p -> p `mod` 10 == 7) primes)
    pn = product p7s
    q l = case l `mod` 4 of 
        0 -> 7 -- xx1
        1 -> 1 -- xx7
        2 -> 3 -- xx9
        3 -> 9 -- xx3

answer4 :: Int -> Integer
answer4 k = pn * (5*(ppartC' !! k) + qpart k)
    where
    p7s = take k (filter (\p -> p `mod` 10 == 7) primes)
    pn = product p7s

qpart :: Int -> Integer
qpart k = sum (map go [0..k])
    where
    go :: Int -> Integer
    go l = (*((-1)^l))
         $ (q l-5) * (fromIntegral k `choose` fromIntegral l)
    q l = case l `mod` 4 of 
        0 -> 7 -- xx1
        1 -> 1 -- xx7
        2 -> 3 -- xx9
        3 -> 9 -- xx3

ppart :: Int -> Integer
ppart k = sum (map gogo [0..k]) 
    where
    gogo :: Int -> Integer
    gogo l = (*((-1)^l)) $ sum (map (div pn . product) $ choosing l p7s)
    p7s = take k (filter (\p -> p `mod` 10 == 7) primes)
    pn = product p7s

ppart' :: Int -> Integer
ppart' k = sum $ map (div pn . product) $ Data.List.subsequences $ map negate p7s
    where
    p7s = take k (filter (\p -> p `mod` 10 == 7) primes)
    pn = product p7s

ppart'' :: Int -> Integer
ppart'' k = sign $ sum $ map product $ Data.List.subsequences $ map negate p7s
    where
    p7s = take k (filter (\p -> p `mod` 10 == 7) primes)
    sign = if odd k then negate else id

ppartC :: Int -> Integer
ppartC k = abs $ sum $ map product $ Data.List.subsequences $ map negate p7s
    where p7s = take k (filter (\p -> p `mod` 10 == 7) primes)

ppartC' :: [Integer]
ppartC' = scanl go 1 $ filter (\p -> p `mod` 10 == 7) primes
    where go !f p = p * f - f

-- * Utils
choose :: Integer -> Integer -> Integer
n `choose` k = product [1+max k (n-k)..n] `div` product [1..min k (n-k)]


-- sum_(i=0)^n(i r + p)
linearSum :: Integral a => a -> a -> a -> a
linearSum r p n = (n+1) * (2*p + r*n) `div` 2

linearSumLT :: Integral a => a -> a -> a -> a
linearSumLT r p n = linearSum r p $ (n-p) `div` r

divides :: Integral a => a -> a -> Bool
a `divides` b = b `mod` a == 0


choosing 0 _ = [[]]
choosing _ [] = []
choosing n (a:as) = (if length as >= n then (choosing n as ++) else id)
                  $ map (a:) (choosing (n-1) as)

primes :: [Integer]
primes = map head (scanl minus [2..] [[p, p+p..] | p <- primes])

minus :: Ord a => [a] -> [a] -> [a]
[] `minus` _ = []
a `minus` [] = a
(a:as) `minus` (b:bs) = case compare a b of
    EQ ->         as  `minus`    bs
    LT -> a : (   as  `minus` (b:bs))
    GT ->      (a:as) `minus`    bs
