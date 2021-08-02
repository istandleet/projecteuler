{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
import qualified Data.List as List
import qualified Data.Set as Set

main :: IO ()
main = print $ answer $ 10^13

-- actually tail fibs
fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

answer :: Integer -> Integer
answer n = answert (succ n) n

answert :: Integer -> Integer -> Integer
answert _ 0 = 1
answert l n = 2^easies + sum (fmap recurse solos)
    where 
    dfs = V.fromList $ takeWhile (\z -> z<l&&z<=n) fibs 
    easies = pred $ length $ V.takeWhile (<=n) $ V.scanl (+) 0 dfs
    (easyfs,solos) = V.splitAt easies dfs
    -- recurse z = sum $ map succ $ let n' = n-z in answert z n'
    recurse z = answert z (n-z)

fs n = let l = map answer [0..n] in zipWith (-) l (0:l)


fs' n = let l = map (length . answerl) [0..n] in zipWith (-) l (0:l)

setfs n = let l = map answerl [0..n] in zipWith go l (tail l)
    where go b a = Set.fromList a `Set.difference` Set.fromList b
    

answerl :: Integer -> [[Integer]]
answerl n = answerlt (n+1) n 

answerlt :: Integer -> Integer -> [[Integer]]
answerlt _ 0 = [[]]
answerlt l n = powerset (V.toList easyfs) <> foldMap recurse solos
    where 
    dfs = V.fromList $ takeWhile (\z -> z<l&&z<=n) fibs 
    easies = pred $ length $ V.takeWhile (<=n) $ V.scanl (+) 0 dfs
    (easyfs,solos) = V.splitAt easies dfs
    recurse z = map (z:) $ let n' = n-z in answerlt z n'

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

