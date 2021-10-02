{-
TODO solve the repdigit modulo problem for large n (10^15)
-}
module Main where

main = print answer

_MOD = 1000000007

answer :: Integer
answer = sum (map bigs' $ drop 2 $ take 91 fibs) `mod` _MOD

bigs n = sum $ map s [1..n]
-- > quickCheck $ \(Positive n) -> bigs' n == bigs n
bigs' :: Integer -> Integer
bigs' n = 
    let (q,r) = n `divMod` 9 
        -- qpart = sum $ take (fromIntegral q) $ map (subtract 9) 
        --       $ map (*54) $ iterate (*10) 1
        qpart = subtract (9*q) $ (*54) $ sum $ take (fromIntegral q) 
              $ iterate (*10) 1
     in qpart + sum (map s [n-r+1..n])
     
s n = let (q,r) = n `divMod` 9 in (r+1)*10^q-1

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fastOnes n modulus = sum (take n $ iterate (*10) 1) `mod` modulus
