{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Scientific
import qualified Data.List


-- 0.229247220395408
-- 0.249336983659159

main = do
    let block = div ( 2^1000) (10^12)
        Just ix = Data.List.findIndex ((<=block).fst) $ cps !! 1000
        (big,hip) = cps !! 1000 !! pred ix
        (low,lop) = cps !! 1000 !! ix
        answer = lop + (hip-lop) * fromInteger (block-low) * recip (fromInteger $ big-low)
    putStrLn $ formatScientific Fixed (Just 15) $ unsafeFromRational answer

b = 0.6

p :: Int -> Rational -> (Rational, [(Int,Int)])
p 1 d
  | d >= 1    = (1   , [])
  | d >= 0.5  = (b , [])
  | otherwise = (0   , [])
p n d = 
  let s = floor $ d * 2^n 
   in case s of 
        0 -> (0, [])
        1 -> (b^n, [(0,2)])
        _ -> 
         let ws = 
                [ (b * fst (p (n-1) hi) + (1-b) * fst (p (n-1) lo), (div(s-w)2,div(s+w)2))
                | w <- [0..s]
                , let hi = fromIntegral (s+w) * recip (2^n)
                      lo = fromIntegral (s-w) * recip (2^n)
                ]
             ans = maximum $ map fst ws
          in (ans, map snd $ filter ((==ans) . fst) ws)

pp :: [(Rational, [(Int,Int)])] -> IO ()
pp = mapM_ (\(i,(s,l)) -> print (i,unsafeFromRational s,l)) . reverse . zip [0::Int ..]

tower n = map (p n . (/2^n)) [0..2^n]

-- critical points have only one good path and create themselves all fibonacci like
cps = go [(1,1),(0,0)]
    where
    go ls = ls
          : go (zipWith mk (head ls : ls) (ls ++ [(0,0)]))
    mk (a,wp) (c,lp) = (a+c,b*wp+(1-b)*lp)
