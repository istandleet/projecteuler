module Main (main) where

import Data.Foldable
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Primes

main :: IO ()
main = print $ f (10^8) -- 83985379 in 1.42s on 2.6 GHz 6-Core Intel Core i7

f :: Integer -> Integer
f = mo . (*2) . lcmlt

mo :: Integer -> Integer
mo = flip mod 1000000007

lcmlt' :: Integer -> Integer
lcmlt' n = foldl1 lcm [1..n]

-- > quickCheck $ \(Positive n) -> n==1 || lcmlt n == lcmlt' [1..n]
lcmlt :: Integer -> Integer
lcmlt n = foldl' (\j k -> mo $ j*k) 1 -- `mo . product` blows up the cpu time >20x
        $ map (\p -> p^integerLogBase p n) 
        $ primesLt n

primesLt :: Integer -> [Integer]
primesLt n = map unPrime [nextPrime 2.. precPrime n]

