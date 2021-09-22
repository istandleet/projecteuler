module Digits where
import Data.Foldable

toDigits :: Integral a => a -> a -> [a]
toDigits b x = reverse (go x)
    where
        go 0 = []
        go n = let (rs,f) = n `divMod` b in f : go rs

-- fromDigits :: Integral a => a -> [a] -> a
fromDigits b = foldl' (\acc n -> acc * b + n) 0