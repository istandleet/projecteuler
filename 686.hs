{-
Hack Solution
-}

{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Bits
import Math.NumberTheory.Logarithms -- from integer-logarithms

main = print $ ps' 123 !! pred 678910 

p :: Integer -> Int -> Integer
p a n = ps a !! pred n

ps :: Integer -> [Integer]
ps a = map fst 
     $ filter (startsWith a . snd)
     $ zip [0..] $ iterate (`shiftL` 1) 1

ps' :: Integer -> [Integer]
ps' = map (floor . snd) . filter f . bounds' where f (a,b) = ceiling a == floor b

bounds' :: Integer -> [(Double, Double)]
bounds' a = flip map [1..] $ \ n -> ( l + fromIntegral n * c , r + fromIntegral n * c )
    where
    !l = logBase 2 (fromIntegral  a   )
    !r = logBase 2 (fromIntegral (a+1))
    !c = logBase 2 10

-- startsWith :: Integer -> Integer -> Bool
-- startsWith a n = a == numDigits a `firstDigits` n
startsWith a = 
    let nda = numDigits a in \n -> 
        let d = 10^max 0 (numDigits n - nda) 
         in a * d <= n && n < (a+1)*d

firstDigits :: Int -> Integer -> Integer
firstDigits d n =
    let m = 10^max 0 (numDigits n -d) 
     in n `quot` m

numDigits :: Integer -> Int
numDigits n = succ $ integerLog10 n

diffs :: Num c => [c] -> [c]
diffs ls = zipWith (-) (tail ls) ls

-- take 100 $ diffs $ ps 12
-- [73,10,10,10,73,10,10,73,10,10,73,10,10,10,73,10,10,73,10,10,10,73,10,10,73,10,10,73,10,10,10,73,10,10,73,10,10,10,73,10,10,73,10,10,83,...

-- take 100 $ diffs $ ps 123
-- [289,196,289,196,485,196,289,196,289,196,289,196,485,196,289,196,289,196,289,196,485,196,289,196,289,196,289,196,485,485,196,289,196,289,196,485,485,196,289,196,289,196,485,485,196,289,196,289,196,485,485,196,289,196,289,196,289,196,485,196,289,196,289,196,289,196,485,196,289,196,289,196,289,196,485,196,289,196,289,196,289,196,485,485,196,289,196,289,196,485,485,196,289,196,289,196,485,485,196,289] 