
{-
    NB: This does not fully solve the problem. While `covercosts` appears to implement a correct
    algorithm, `f` only works in the naive case, where you can greedily take the most common
    remaining PF. 
-}

{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Math.NumberTheory.Primes -- arithmoi

import GHC.Exts (the, groupWith)

main :: IO ()
main = print $ f (10^6)

f :: Integer -> Double
f n = 
    let (done, remnant) = requiredPrimes [3,13..n]
        naive = naiveCover remnant
     in logsum $ map unPrime $ Set.toList done ++ naive

logsum :: [Integer] -> Double
logsum = sum . map (log . fromIntegral)

-- requiredPrimes :: [Integer] -> Set Prime
requiredPrimes = requiredCoverElems . map toprimes 

requiredCoverElems :: Ord a => [Set a] -> (Set a, [Set a])
requiredCoverElems ss =
    let (as, bs) = Data.List.partition ((==1) . length) ss
        inits = mconcat as
        remnant = filter (Set.disjoint inits) bs
     in (inits, remnant)

covers, fullcovers :: (Foldable t, Ord a) => Set a -> t (Set a) -> Bool
covers s = not . any (Set.disjoint s)
fullcovers s is = s `covers` is && not (any (\n -> covers (Set.delete n s) is) s) 

toprimes :: Integer -> Set (Prime Integer)    
toprimes = Set.fromList . map fst . factorise

-- covercosts (map (Set.map unPrime) $ snd $ requiredPrimes [3,13..2800]) == 13.22248559711485
covercosts :: [Set Integer] -> Double
covercosts ss = 
    let nc = naiveCover ss
     in go [] ss (logsum nc) (Set.toList $ mconcat ss)
    where 
    go req remnant !upperbound [] = logsum req `min` upperbound
    go req [] !upperbound _ = logsum req `min` upperbound
    go req remnant !upperbound _ 
        | logsum req > upperbound = upperbound
        -- | logsum (req ++ lowerbound remnant) > upperbound = upperbound
    go req remnant !upperbound (banned:cs) = 
        let -- find all the places where you now have a forced decision
            (newps, togo) = requiredCoverElems $ map (Set.delete banned) remnant 
            ifRequired = go (banned:req) (filter (Set.notMember banned) remnant) upperbound cs
            ifSkipped = go (Set.toList newps <> req) (filter (Set.disjoint newps) togo) upperbound cs

            lbifskipped = logsum 
                       $ req 
                      <> Set.toList newps
                      <> lowerbound togo

         in if lbifskipped > upperbound then ifRequired else min ifRequired ifSkipped
         
forcedCover :: Double -> Integer -> [Set Integer] -> Maybe [Set Integer]
forcedCover target c remnant =
    -- first, determine if c is even possible, by finding forced moves
    let (newps, togo) = requiredCoverElems $ map (Set.delete c) remnant
        target' = logsum $ Set.toList newps 
     in if target' > target 
         then Just $ filter (Set.notMember c) remnant
         else Nothing

naiveCover :: Ord a => [Set a] -> [a]
naiveCover [] = []
naiveCover ss | all null ss = []
naiveCover ss = 
    let (next, _) = head $ Data.List.sortOn (negate . snd) $ count $ foldMap Set.toList ss
     in next : naiveCover (filter (Set.notMember next) ss)

-- fails for `snd $ requiredPrimes [3,13..2800]`
greedyCover :: Ord a => [Set a] -> [a]
greedyCover ss = go (Set.toAscList $ mconcat ss) ss
    where
    go _ [] = []
    go (i:is) remnant = i : go is (filter (Set.notMember i) remnant)

lowerbound :: Ord a => [Set a] -> [a]
lowerbound ss = go (length ss) $ zip ems $ Data.List.sortOn negate $  map snd $ count $ foldMap Set.toList ss
    where
    ems = Set.toAscList $ mconcat ss
    go !n [] = [] 
    go !n ((m,c):cs) 
        | n <= 0 = []
        | otherwise = m : go (n-c) cs

count :: Ord a => [a] -> [(a, Int)]
count xs = [(the x, length x) | x <- xs, then group by x using groupWith]
