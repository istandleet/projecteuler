{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.List
import qualified Data.Sequence as Seq

import qualified Math.NumberTheory.Primes as P


main :: IO ()
main = print $ answer' 800800

answer :: Integer -> Int
answer constant = length $ reducing pass' $ map (fromInteger . P.unPrime) $ P.primes
    where pass' = pass $ fromInteger constant

pass c = let con = c * log c in \a b -> a * log b + b * log a < con

reducing :: (t -> t -> Bool) -> [t] -> [(t, t)]
reducing f [] = []
reducing f [x] = []
reducing f (p:ps) = let ps' = takeWhile (f p) ps in map (p,) ps' ++ reducing f ps'

answer' :: Integer -> Int 
answer' constant = reducingl f $ Seq.fromList $ takeWhile (f 2) $ map (fromInteger . P.unPrime) $ P.primes
    where f = pass $ fromIntegral constant

reducingl :: (a -> a -> Bool) -> Seq.Seq a -> Int
reducingl f = go 0
    where 
    go !n Seq.Empty = n
    go !n (p Seq.:<| ps) = let ps' = Seq.dropWhileR (not . f p) ps in go (n+length ps') ps'
