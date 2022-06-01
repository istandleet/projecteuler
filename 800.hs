{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Function
import Data.Foldable
import qualified Data.List
import qualified Data.List.Ordered as OL
import qualified Data.Set as Set

import qualified Math.NumberTheory.Primes as P
import Math.NumberTheory.Roots -- cabal install integer-roots --lib


main :: IO ()
main = print $ answer 800800

answer :: Integer -> Int
answer constant = length $ reducing pass' $ map (fromInteger . P.unPrime) $ P.primes
    where pass' = pass $ fromInteger constant

pass c = let con = c * log c in \a b -> a * log b + b * log a < con

reducing f [] = []
reducing f [x] = []
reducing f (p:ps) = let ps' = takeWhile (f p) ps in map (p,) ps' ++ reducing f ps'