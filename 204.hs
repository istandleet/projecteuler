module Main where

main :: IO ()
main = print answer

answer :: Int
answer = length $ hammingNumbersOfTypeLessThan 100 (10^9)

hammingNumbersOfTypeLessThan :: Integer -> Integer -> [Integer]
hammingNumbersOfTypeLessThan n bigN = foldr go [1] $ takeWhile (<=n) primes
    where go r = foldMap $ \hamm -> takeWhile (<=bigN) $ iterate (*r) hamm
    
primes :: [Integer]
primes = map head (scanl minus [2..] [[p, p+p..] | p <- primes])

minus :: Ord a => [a] -> [a] -> [a]
[] `minus` _ = []
a `minus` [] = a
(a:as) `minus` (b:bs) = case compare a b of
    EQ ->         as  `minus`    bs
    LT -> a : (   as  `minus` (b:bs))
    GT ->      (a:as) `minus`    bs
