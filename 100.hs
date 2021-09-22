{-# language BangPatterns #-}
{-# language TypeApplications #-}
import qualified Data.List 
import Data.Maybe
import System.Environment
import Data.Ratio
import Math.NumberTheory.Primes

main = interact' "-lt for list less than" main'

interact' :: String -> ([String] -> String) -> IO ()
interact' name _main = getArgs >>= putStrLn . (\s -> if null s then name else s) . _main 

main' args = case args of
    [n] -> show $ answer' $ read $ n
    ["-lt",n] -> pshow $ takeWhile ((< (read n)) . snd) fiftyProbs
    _ -> ""

answer n = Data.List.find ((>n) . snd) fiftyProbs
answer' n = head $ dropWhile (< n) $ mapMaybe getValidT [1..]

-- pshow :: [(Integer,Integer)] -> String
pshow xs = unlines 
         $ (sep $ ["Blue","Blu-1","Total","Tot-1","Primes"])
         : map s xs
  where s (b,t) =  sep $ map show [b,b-1,t,t-1] ++ [fshow (b * (b-1))]
        sep = Data.List.intercalate "\t| " 
        fshow = pfshow . factorise
pfshow :: [(Integer,Int)] -> String
pfshow = Data.List.intercalate "*" 
       . map (\(p,n) -> if n == 1 then show p else concat [show p,"^",show n])
      
-- fiftyProbs = mapMaybe findBlueCount [1..]
fiftyProbs :: [(Integer,Integer)]
fiftyProbs = go 4 1 2
  where
    go acc blue total = case compare (2 * blue * (blue - 1)) (total * (total - 1)) of
        LT -> go acc (succ blue) total
        EQ -> (blue,total) : go (blue % total) (succ blue) (truncate $ acc * toRational total)
        GT -> go acc blue (succ total)

findBlueCount n = Data.List.find (uncurry is50Prob) $ zip [1..n] (repeat n)
      
prob blue total = blue * (blue - 1) / (total * (total - 1))
is50Prob blue total = 2 * blue * (blue - 1) == total * (total - 1)

getValidT :: Integer -> Maybe Integer
getValidT b = let l  = 2 * b * (b - 1)
                  n' = truncate (sqrt $ fromInteger l)
                  n = succ n'
               in if n' * n == l then Just n else Nothing