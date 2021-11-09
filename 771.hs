-- see https://hackmd.io/hntUqfygT8eeFq9Pq3SEUw

module Main where

import Control.Applicative
import Data.Function
import qualified Data.List
import qualified Data.Set as Set
import Data.Ratio
import Math.NumberTheory.Roots
import Math.NumberTheory.Logarithms

main :: IO ()
main = do 
    print $ g' 10000
    print $ g'' 10000
    print $ g'' (10^18) `mod` 1000000007

findDuplicates :: Ord a => [a] -> [a]
findDuplicates = go mempty 
    where 
    go acc [] = []
    go acc (x:xs) = (if x `Set.member` acc then (x:) else id) $ go (Set.insert x acc) xs

g :: Integer -> Int
g = length . s

g' :: Integer -> Integer
g' n = sum (map (fromIntegral . subfives . length) $ m n)

s :: Integer -> [[Integer]]
s n | n<5=[]
s n = fivelongs n ++ concatMap go (s $ n - 1)
    where 
    canadd (a:b:_) = check n a b
    go l = l : [n:l | canadd l]

m :: Integer -> [[Integer]]
m n | n<5=[]
m n = news ++ olds
    where 
    olds = map go (m $ n - 1)
    news = filter (\l -> not $ any (l `Data.List.isPrefixOf`) olds) 
         $ fivelongs n
    go l@(a:b:_) = if check n a b then n:l else l

infiniteGeometricSequences = [[p^n | n<- [0..]] | p <- [2..]]

igsThatImpact :: Integral a => a -> [[a]]
igsThatImpact bigN = 
    [ [p^n | n<- takeWhile (\n -> p^n < bigN) [0..]] 
    | p <- takeWhile (\p -> p^5 < bigN) [2..]]

fivelongs :: Integer -> [[Integer]]
fivelongs n = reverse [n-4..n] : do
    a <- [4..n-2] -- should be able to increase the lower bound a lot
    b <- movedown n a
    c <- movedown a b 
    d <- movedown b c 
    pure [n,a,b,c,d]
    where 
    go a = Nothing

check :: Integer  -> Integer -> Integer -> Bool
check a b c = abs (b^2-a*c) <= 2

movedown :: Integer -> Integer -> [Integer]
movedown a b = let r = b*b `div` a in filter (check a b) [max 1 $ r-2..min (r+2) (pred b)]

g'' n = rrcomp n + natcomp n + igscomp n + fgscomp n + weirdcomps n
natcomp :: Integer -> Integer
natcomp = subfives
igscomp :: Integer -> Integer
igscomp n = sum
    [ n `div` (r^k)
    | r <- [2..integerRoot 4 n]
    , k <- [4..integerLogBase r n]
    ]
fgscomp :: Integer -> Integer
fgscomp n = sum
    [ n `div` (p^k)
    | q <- [2..integerRoot 4 n]
    , p <- [q+1..integerRoot 4 n]
    , gcd p q == 1
    , k <- [4..integerLogBase p n]
    ]

rr a0 a1 r1 r2 = fix $ \g -> a0 : a1 : zipWith (\amin2 amin1 -> r1*amin1 + r2*amin2) g (tail g)

rrcomp :: Integer -> Integer 
rrcomp n = 
    subfiveslt n (rr 1 2 1 1) -- fibs
  + subfiveslt n (rrseqDown 3 4)
  + subfiveslt n (rrseqUp 3 2)
  + subfiveslt n (rrseqDown 2 3)
  + sum (takeWhile (>0) $ map (\r -> subfiveslt n $ rrseqUp   r r) [2..])
  + sum (takeWhile (>0) $ map (\r -> subfiveslt n $ rrseqDown r r) [3..]) -- 2 only goes up
  where
    rrseqUp a1 r = rr 1 a1 r 1
    rrseqDown a1 r = rr 1 a1 r (-1)
  


weirdcomps :: Integer -> Integer
weirdcomps n = (sum $ map (subfiveslt n) 
    [ [1,2,6,19,60]
    , [1,2,6,17,48]
    , [1,2,4,9,20]
    , [1,2,4,7,12]
    , [1,2,3,5,9,16]
    , [1,2,3,4,6,9]
    ]) + prefixfor1_2igs n
prefixfor1_2igs n = max 0 (fromIntegral $ length (takeWhile (<= n) $ 1 : iterate (*3) 2) - 4)
    
s' :: Integer -> [[Integer]]
s' = nats <> sInteresting
nats :: Integer -> [[Integer]]
nats n = dosubfiveslt n [1..n]
sInteresting = rrcomps <> igss <> fgss <> weirds
rrcomps :: Integer -> [[Integer]]
rrcomps n = 
     dosubfiveslt n fibs
  ++ dosubfiveslt n (rrDown 3 4)
  ++ dosubfiveslt n (rrUp 3 2)
  ++ dosubfiveslt n (rrDown 2 3)
  ++ concat (takeWhile (not . null) $ map (\r -> dosubfiveslt n $ rrUp   r r) [2..])
  ++ concat (takeWhile (not . null) $ map (\r -> dosubfiveslt n $ rrDown r r) [3..]) -- 2 only goes up
  where
    rrUp a1 r = rr 1 a1 r 1
    rrDown a1 r = rr 1 a1 r (-1)

weirds :: Integer -> [[Integer]]
weirds n = concatMap (dosubfiveslt n)
    [ [1,2,6,19,60]
    , [1,2,6,17,48]
    , [1,2,4,9,20]
    , [1,2,4,7,12]
    , [1,2,3,5,9,16]
    , [1,2,3,4,6,9]
    ] ++ prefixsfor1_2igs n 
prefixsfor1_2igs n = dropWhile ((<5) .length) $ Data.List.inits ( takeWhile (<=n) $ 1 : iterate (*3) 2 )

fgss :: Integer -> [[Integer]]
fgss n = 
    [ [ i*q^(k-j)*p^j| j <- [0..k]]
    | q <- [2..integerRoot 4 n]
    , p <- [q+1..integerRoot 4 n]
    , gcd p q == 1
    , k <- [4..integerLogBase p n]
    , i <- [1..n `div` (p^k)]
    ]
igss :: Integer -> [[Integer]]
igss n = 
    [ [ i*r^j| j <- [0..k]]
    | r <- [2..integerRoot 4 n]
    , k <- [4..integerLogBase r n]
    , i <- [1..n `div` (r^k)]
    ]

-- | Given a sequence of length `l`, this is how many subsequences
-- have at least `l` elements
subfives :: Integral a => a -> a
subfives l | l < 5 = 0
subfives l = (l^2 - 7*l + 12) `div` 2

subfiveslt :: Integer -> [Integer] -> Integer
subfiveslt n = subfives . fromIntegral  . length . takeWhile (<= n)

dosubfiveslt :: Ord a => a -> [a] -> [[a]]
dosubfiveslt n l = 
    [ take j $ drop i l 
    | j <- [5..applicable]
    , i<-[0..applicable-j]]
    where applicable = length $ takeWhile (<=n) l

-- util
moveup :: Integer -> Integer -> [Integer]
moveup a b = let r = b*b `div` a in filter (check a b) [max 1 $ r-2..r+2]

isTrivial :: [Integer] -> Bool 
isTrivial g = (==1) $ product $ zipWith (-) g (tail g)

isStrictFib :: [Integer] -> Bool
isStrictFib g = g `Data.List.isInfixOf` reverse (takeWhile (<= maximum g) fibs)

isLooseFib :: [Integer] -> Bool
isLooseFib g = g `Data.List.isSubsequenceOf` reverse (takeWhile (<= maximum g) fibs)

isGeometric :: [Integer] -> Bool
isGeometric g = (==1) $ length $ Data.List.nub $ zipWith (%) g (tail g)

isClassified g = isTrivial g || isStrictFib g || isGeometric g

works :: [Integer] -> Bool
works l = and $ zipWith3 check l (tail l) (tail$tail l)

continue :: [Integer] -> [[Integer]]
continue l@(a:b:_) = map (:l) $ moveup b a  

fibs = 1 : 2 : zipWith (+) fibs (tail fibs)