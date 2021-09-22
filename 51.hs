{-# language BangPatterns #-}
{-# language TypeApplications #-}
import Math.NumberTheory.Primes
import Digits
import qualified Data.Map as Map

main = print answer

answer = head $ filter (isNPrimeValueTheory 8) primes

isNPrimeValueTheory n p = not $ null $ 
    [ d
    | d <- ds
    , d <= 10-n
    , length (filter isPrime $ map (rep d) [succ d..9]) == fromIntegral ( pred n )
    ]
    where 
      ds = toDigits 10 p
      rep c i = fromDigits 10 $ map (\x -> if x == c then i else x) ds