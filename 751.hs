{-
Solution converges to answer in 6 iterations. 
-}
module Main where

import Data.Scientific

main = putStrLn $ formatScientific Fixed (Just 24) answer

answer :: Scientific
answer = go $ iterate test 2.2
    where
    go (d:d':ds) = if abs (d - d') < scientific 1 (-24) then d else go ds

test = sum . toTao . getTaoDigits 24

getTaoDigits n = go (succ n) . as
  where
  go i (l:ls) = if i <= 0 then [] else l : go (i-length(show l)) ls

bs :: Scientific -> [Scientific]
bs x = x : bs (fromIntegral (floor x) * (x - fromIntegral (floor x) + 1))

as :: Scientific -> [Integer]
as = map floor . bs

toTao :: [Integer] -> [Scientific]
toTao (a:as) = fromIntegral a : go 0 as
    where
    go n [] = []
    go n (x:xs) = let d = length (show x) in scientific x (negate$d+n) : go (d+n) xs