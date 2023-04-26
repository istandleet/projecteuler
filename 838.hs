
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import GHC.Exts (the, groupWith)

import Math.NumberTheory.Primes -- arithmoi

import Data.Time

main :: IO ()
main = print $ f (10^6)

-- | Actual solution. Runs in ~1.5s in ghci
f :: Integer -> Double
f n = 
    let (done, remnant) = requiredPrimes [3,13..n]
     in logsum (map unPrime $ Set.toList done) + covercosts (map (Set.map unPrime) remnant)

-- | Original solution. Works by coincidence for 10^6
f' :: Integer -> Double
f' n = 
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

-- | Remove primes from your set which are strictly dominated. A prime is strictly dominated
-- if it only occurs once and there is another element in the set which is lower than it.
dominatedPrimes :: Ord a => [Set a] -> (Set a, [Set a])
dominatedPrimes ss = if any (==1) counts then requiredCoverElems $ map filterDominanted ss else (mempty, ss)
    where 
    counts = Map.fromList $ count $ foldMap Set.toList ss
    filterDominanted ps = Set.filter (\p -> p == minimum ps || counts Map.! p > 1) ps

-- initialRequirements $ snd $ requiredPrimes [3,13..2800]
initialRequirements :: Ord a => [Set a] -> (Set a, [Set a])
initialRequirements ss = 
    let (ps, remnant) = requiredCoverElems ss
        (dp, remnant') = dominatedPrimes remnant
     in if null dp
            then (ps, remnant')
            else let (r, s) = initialRequirements remnant' in (ps <> dp <> r, s) -- this recursion is not actually required, since dominatedPrimes calls `requiredCoverElems`, which is ~idempotent. But that's just what's happening now, this is more future proof.

covers, fullcovers :: (Foldable t, Ord a) => Set a -> t (Set a) -> Bool
covers s = not . any (Set.disjoint s)
fullcovers s is = s `covers` is && not (any (\n -> covers (Set.delete n s) is) s) 

toprimes :: Integer -> Set (Prime Integer)    
toprimes = Set.fromList . map fst . factorise

-- covercosts (map (Set.map unPrime) $ snd $ requiredPrimes [3,13..2800]) == 13.22248559711485
-- covercosts [Set.fromList [2,1000], Set.fromList [3,1000]] == 1.791759469228055
covercosts :: [Set Integer] -> Double
covercosts ss = 
    let (ps, remnant) = initialRequirements ss
     in go (Set.toList ps) remnant (Set.toList $ mconcat remnant Set.\\ ps)
    where 
    upperbound = logsum $ naiveCover ss
    go req remnant [] = logsum req `min` upperbound
    go req [] _ = logsum req `min` upperbound
    go req remnant ps
        | logsum req > upperbound = upperbound
        -- | logsum (req ++ lowerbound remnant) > upperbound = upperbound
    go req remnant (banned:cs) = 
        let -- find all the places where you now have a forced decision
            (newps, togo) = requiredCoverElems $ map (Set.delete banned) remnant

            ifRequired = go (banned:req) (filter (Set.notMember banned) remnant) cs
            ifSkipped = go (Set.toList newps <> req) togo cs

            lbifskipped = logsum 
                       $ req 
                      <> Set.toList newps
                      <> lowerbound togo

         in if lbifskipped > upperbound then ifRequired else min ifRequired ifSkipped

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

timeit x = do
    a <- getCurrentTime
    print x 
    b <- getCurrentTime 
    print (b `diffUTCTime` a)
