{-# LANGUAGE TransformListComp #-}

module Main where

import Data.Maybe
import Data.Monoid
import qualified Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts (the, groupWith)

main :: IO ()
main = print answer

f30 m = fromIntegral $ cache IntMap.! fromIntegral m
    where cache = IntMap.fromList f30Gen

answer :: Integer
answer = sum $ map fromIP $ partitions 20 2 12

fromIP :: [Integer] -> Integer
fromIP ms = product (map f30 ms) * fromIntegral (product [12-length ms+1..12]) 
      `div` product (map (fact . fromIntegral . length) $ Data.List.group ms)

-- * Generating using 3-2 and 5-2 substitution
f30Gen :: [(Int,Int)]
f30Gen = [(the l, length l) | l <- map IntSet.size $ Set.toList all30s, then group by l using groupWith]

all30s :: Set IntSet
all30s = go mempty $ Set.fromList all30twogons
    where
    go seen new | null new = seen
    go seen new =
        let seen' = new <> seen
            new' = foldMap split30 new 
         in go seen' $ new' Set.\\ seen'

all30twogons :: [IntSet]
all30twogons = map (foldMap $ \i -> IntSet.fromList [i,i+15]) $ powerset [0..14]

split30 :: IntSet -> Set IntSet
split30 candles = Set.fromList $ foldMap splitter $ IntSet.toList candles
    where 
    splitter m = 
        let without = IntSet.delete m candles
         in map (without <>) $ filter (IntSet.null . IntSet.intersection without) [threeTwoSub m, fiveTwoSub m]
         
threeTwoSub :: Int -> IntSet
threeTwoSub m = let f = flip mod 30 in IntSet.fromList $ map f [m-5,m+5] 
fiveTwoSub :: Int -> IntSet
fiveTwoSub m = let f = flip mod 30 in IntSet.fromList $ map f [m-9,m-3,m+3,m+9]

-- * utils
fact n = product [1..n]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

partitions n resid 1 
    | n >= resid = [[n]]
    | otherwise  = []
partitions n resid _buckets
    | resid > n = []
    | n < 2*resid = [[n]]
partitions n resid buckets = map (resid:) (partitions (n-resid) resid (buckets-1))
                          ++ partitions n (resid+1) buckets

choose :: Integer -> Integer -> Integer
n `choose` k = product [1+max k (n-k)..n] `div` product [1..min k (n-k)]

-- * Exploration
isBalanced :: Int -> [Int] -> Bool
isBalanced n ms = 
    let (x,y) = centerOfGravity n ms
     in abs x < 1e-9 && abs y < 1e-9

centerOfGravity :: Int -> [Int] -> (Double, Double)
centerOfGravity n ms = 
    (sum $ map x ms, sum $ map y ms)
    where 
    theta m = 2 * pi * m / fromIntegral n
    x = cos . theta . fromIntegral
    y = sin . theta . fromIntegral

findBalance :: Int -> [Int] -> Maybe Int
findBalance n ms = 
    let (x,y) = centerOfGravity n ms
     in if x*x+y*y > 1 then Nothing 
            else find (\i -> i `notElem` ms && isBalanced n (i:ms)) [0..pred n]

find p [] = Nothing 
find p (x:xs) = if p x then Just x else find p xs

interesting :: Int -> Int -> [IntSet]
interesting n m = filter (not . IntSet.null) $ map (ngonBgon n 5) $ 
    map (ngonBgon n 3) $ map (ngonBgon n 2) $ Set.toList $ bruteForce n m

bruteForce :: Int -> Int -> Set.Set IntSet
bruteForce n m = Set.fromList $ map IntSet.fromList $ mapMaybe go $ possibilities n (m-1)
    where 
    go :: [Int] -> Maybe [Int]
    go xs = (:xs) <$> findBalance (fromIntegral n) xs

possibilities :: Int -> Int -> [[Int]]
possibilities n m = map (0:) $ generate (m-1) 1
    where
    generate remaining start 
        | remaining == 0 = [[]]
        | start + remaining >= n = []
        | otherwise = map (start:) (generate (pred remaining) (succ start)) 
                    ++ generate remaining (succ start)

ngonBgon :: Int -> Int -> IntSet -> IntSet
ngonBgon n gon ms = IntSet.filter (not . inagon) ms
    where
    inagon m = 
        let partners = map (\i -> (m + i*n `div` gon) `mod` n) [1..gon-1]
         in all (`IntSet.member` ms) partners

fHard :: Int -> Int -> Int
fHard n m = length $ bruteForceReal n m 

bruteForceReal :: Int -> Int -> Set.Set IntSet
bruteForceReal n m = Set.fromList $ map IntSet.fromList $ filter (isBalanced n) $ possibilitiesReal n m

possibilitiesReal :: Int -> Int -> [[Int]]
possibilitiesReal n m = generate m 0
    where
    generate remaining start 
        | remaining == 0 = [[]]
        | start + remaining > n = []
        | otherwise = map (start:) (generate (pred remaining) (succ start)) 
                    ++ generate remaining (succ start)
