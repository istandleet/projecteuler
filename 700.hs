-- Keywords: ambig
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.Function
import qualified Data.List
import           Data.Set (Set)
import qualified Data.Set as Set



_SEED, _BASE :: Integer
_SEED = 1504170715041707
_BASE = 4503599627370517

main :: IO () -- 1517926517777556
main = meetInTheMiddle (ecoinsDown _SEED _BASE) (ecoinsUp _SEED _BASE) >>= print . sum . map fst . Set.toList
-- main = racingCoins _SEED _BASE

meetInTheMiddle :: Ord a => [a] -> [a] -> IO (Set a)
meetInTheMiddle as bs = do
    tset <- newTVarIO mempty
    concurrently_ (go tset as) (go tset bs)
    readTVarIO tset
    where
    go tset [] = pure ()
    go tset (!x:xs) = do
        continue <- atomically $ do
            s <- readTVar tset
            modifyTVar' tset (Set.insert x)
            return $ x `Set.notMember` s
        when continue $ go tset xs

{-
answer :: Integer -> Integer -> Integer
answer seed base = sum $ map fst $ ecoins seed base

ecoins :: Integer -> Integer -> [(Integer, Integer)] -- coin,n
ecoins = ecoinsUp <> ecoinsDown
-}

racingCoins :: Integer -> Integer -> IO ()
racingCoins seed base = concurrently_ (mapM_ (go "down") $ ecoinsDown seed base) (mapM_ (go "up") $ ecoinsUp seed base)
    where go s p = putStrLn $ unwords [s, show p]

ecoinsUp :: Integer -> Integer -> [(Integer, Integer)] -- coin,n
ecoinsUp seed base = monotonicBy ((<) `on` snd) $ map ((,) <$> id <*> modDiv seed base) [1..seed]

ecoinsDown :: Integer -> Integer -> [(Integer, Integer)] -- coin,n
ecoinsDown seed base = monotonicBy ((<) `on` fst) $ zip (iterate (\coin -> (coin+seed) `mod` base) seed) [1..base]


-- | filters by a function
monotonicBy :: (a -> a -> Bool) -> [a] -> [a]
monotonicBy f [] = []
monotonicBy f (x:xs) = x : go x xs
    where
    go !y [] = []
    go !x (x':xs) = if f x x' then go x xs else x' : go x' xs 

extendedEulidean :: Integral c => c -> c -> (c, c, c)
extendedEulidean 0 b = (b, 0, 1)
extendedEulidean a b = 
    let (q,r) = b `divMod` a
        (g, s, t) = extendedEulidean r a
     in (g, t - q * s, s)

crt2 :: Integral b => (b, b) -> (b, b) -> (b, b)
crt2 (n,m) (n',m') =
    let (r, a, a') = extendedEulidean m m'
     in (n' * a * m + n * a' * m', m*m')
     
-- crt :: [(Integer,Integer)] -> Integer -- crt [(2,3),(3,5),(2,7)] == 23
crt :: (Integral c, Foldable t) => t (c, c) -> c -- crt [(2,3),(3,5),(2,7)] == 23
crt = uncurry mod . foldl' crt2 (0,1)

modDiv :: Integral a => a -> a -> a -> a
modDiv divisor base = let (1,inv,_) = extendedEulidean divisor base in \numerator -> (numerator*inv) `mod` base
