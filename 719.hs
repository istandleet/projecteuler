module Main where

import Data.Foldable
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Math.Combinat (integerSquareRoot)

main :: IO ()
main = print $ answer (10^12)

answer :: Integer -> Integer
answer = sum . map (^2) . filter isSRoot . enumFromTo 9 . integerSquareRoot

isSRoot n = any (==n) 
          $ map (sum . map (fromDigits 10)) 
          $ filter ((>1) . length) 
          $ nonEmptySubsequences 
          $ toDigits 10 (n^2)

nonEmptySubsequences :: [a] -> [[[a]]]
nonEmptySubsequences [x] = [[[x]]]
nonEmptySubsequences (x:xs) = map insert (nonEmptySubsequences xs) ++ map ([x]:) (nonEmptySubsequences xs)
    where 
    insert (y:ys) = (x:y):ys

toDigits :: Integral a => a -> a -> [a]
toDigits b x = reverse (go x)
    where
        go 0 = []
        go n = let (rs,f) = n `divMod` b in f : go rs

-- fromDigits :: Integral a => a -> [a] -> a
fromDigits b = foldl' (\acc n -> acc * b + n) 0