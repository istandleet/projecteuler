module Main where

import Control.Parallel.Strategies
import Data.Bits
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Time (getCurrentTime, diffUTCTime)

__MOD = 1005075251

main = do 
    input <- readFile "p828_number_challenges.txt"
    let ls = map parseLine $ filter (not.null) $ lines input
    print $ sum (answers ls) `mod` __MOD

answers :: [(Int, [Int])] -> [Integer]
answers ls = 
    [ (three_to_n * (fromIntegral g :: Integer)) `mod` __MOD
    | (three_to_n, (target, ns)) <- zip threes ls
    , let g = s target ns
    ] `using` parList rseq
    where
    threes = iterate (\n -> (3*n) `mod` __MOD) 3

timeit x = do
    a <- getCurrentTime
    print x 
    b <- getCurrentTime 
    print (b `diffUTCTime` a)

-- solveLine "211:2,3,4,6,7,25" == 40
solveLine :: String -> Int
solveLine = uncurry s . parseLine

-- parseLine "211:2,3,4,6,7,25" == (211, [2,3,4,6,7,25])
parseLine :: String -> (Int, [Int])
parseLine s = (read n, map read $ words $ map (\c -> if c == ',' then ' ' else c) ns)
    where (n,':':ns) = break (==':') s

combine :: Int -> Int -> [Int]
combine a b = case compare a b of
    LT -> combine b a
    EQ -> [a+b, a*b, 1]
    GT -> case mdiv a b of 
        Nothing -> [a+b, a*b, a-b]
        Just q  -> [a+b, a*b, a-b, q]

mdiv :: Int -> Int -> Maybe Int
-- mdiv a 0 = Nothing -- shouldn't get 0
mdiv a b = let (q,r) = a `divMod` b in if r == 0 then Just q else Nothing

msub :: Int -> Int -> Maybe Int
msub a b = if a > b then Just (a-b) else Nothing

type UsedInts = IntSet

s :: Int -> [Int] -> Int
s target xs = maybe 0 (minimum . map decode . IntSet.toList) $ Map.lookup target $ reaching xs
    where
    decode s = sum [x | (i, x) <- zip [0..] xs, testBit s i]

reaching :: [Int] -> Map Int UsedInts
reaching xs = go 1 $ Map.fromListWith IntSet.union
    [ (x, IntSet.singleton $ setBit 0 i) 
    | (i, x) <- zip [0..] xs
    ]
    where
    -- possibly can add a counter since we don't need to iterate more than `length xs` times
    go !i m | i >= length xs = m 
    go i m = let m' = reachMore m in if m == m' then m else go (succ i) m'

reachMore :: Map Int UsedInts -> Map Int UsedInts
reachMore m = fmap nubs $ 
    Map.unionsWith (<>) $ m : map (uncurry f) (Map.toList m)
    where
    f x s = Map.fromListWith IntSet.union
            [ (y, ss)
            | (x', s') <- Map.toList m
            , let ss = combineUsed s s'
            , not $ IntSet.null ss
            , y <- combine x x'
            ]

combineUsed :: UsedInts -> UsedInts -> UsedInts
combineUsed s1 s2 = IntSet.fromList 
    [ s1 .|. s2
    | s1 <- IntSet.toList s1
    , s2 <- IntSet.toList s2
    , s1 .&. s2 == 0
    ]

nubs :: UsedInts -> UsedInts
nubs s = IntSet.filter (not . isDominated) s
    where isDominated is = any (\is' -> is' .&. is == is') $ IntSet.toList $ fst $ IntSet.split is s

choosing :: [a] -> [(a,[a])]
choosing [] = []
choosing (x:xs) = (x,xs) : map (fmap (x:)) (choosing xs)

{-
-- | Old
-- Doesn't work, ie `s 100 [10, 10] == 0`
s :: Int -> [Int] -> Int
s target = maybe 0 (minimum . map (sum . IntSet.toList) . Set.toList) . Map.lookup target . reaching

reaching :: [Int] -> Map Int (Set IntSet)
reaching xs = go 1 $ Map.fromListWith Set.union [(x, Set.singleton $ IntSet.singleton x) | x <- xs]
    where
    -- possibly can add a counter since we don't need to iterate more than `length xs` times
    go !i m | i >= length xs = m 
    go i m = let m' = reachMore m in if m == m' then m else go (succ i) m'

reachMore :: Map Int (Set IntSet) -> Map Int (Set IntSet)
reachMore m = fmap nubs $ 
    Map.unionsWith (<>) $ m : map (uncurry f) (Map.toList m)
    where
    -- f :: Int -> Set (Set Int) -> Map Int (Set (Set Int))
    f x s = Map.fromListWith Set.union
            [ (y, ss)
            | (x', s') <- Map.toList m
            , let ss = combineUsed s s'
            , not $ null ss
            , y <- combine x x'
            ]

combineUsed :: Set IntSet -> Set IntSet -> Set IntSet
combineUsed s1 s2 = Set.fromList 
    [ s1 <> s2
    | s1 <- Set.toList s1
    , s2 <- Set.toList s2
    , IntSet.disjoint s1 s2
    ]

nubs :: Set IntSet -> Set IntSet
nubs s = Set.filter (\is -> not $ any (`IntSet.isProperSubsetOf` is) s) s

-}