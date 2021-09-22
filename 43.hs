{-# language BangPatterns #-}
{-# language TypeApplications #-}
import qualified Data.List 

main = print answer

answer = sum . map fromdigits $ filter propertiesVerbose $ pandigitals 0 9

digits n | n == 0 = []
         | n < 1  = digits (-n)
         | otherwise = reverse (go n)
    where 
      go 0 = []
      go x = x `mod` 10 : go (x `div` 10)
      
fromdigits = Data.List.foldl' @[] (\acc n -> 10 * acc + n) 0 

pandigitals a b = Data.List.permutations [a..b]

propertiesVerbose d =
    length d == 10                   
 && fromdigits [d !! 1, d !! 2, d !! 3] .| 2
 && fromdigits [d !! 2, d !! 3, d !! 4] .| 3
 && fromdigits [d !! 3, d !! 4, d !! 5] .| 5
 && fromdigits [d !! 4, d !! 5, d !! 6] .| 7
 && fromdigits [d !! 5, d !! 6, d !! 7] .| 11
 && fromdigits [d !! 6, d !! 7, d !! 8] .| 13
 && fromdigits [d !! 7, d !! 8, d !! 9] .| 17
                     
infixr 9 .|
a .| b = a `mod` b == 0