import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment
import Math.Combinat.Sets

main = print answer

answer = length
       $ filter (\[d1,d2] -> goodDice d1 d2)
       $ choose 2 allDie
       

type Die = Set Int
allDie = map Set.fromDistinctAscList $ choose 6 [0..9]

goodDice :: Set Int -> Set Int -> Bool
goodDice d1 d2 = all (satisfies d1 d2) needs

satisfies s1 s2 (a,b) 
    = has' a s1 && has' b s2
   || has' a s2 && has' b s1
   
has' :: Int -> Set Int -> Bool
has' 6 s = 6 `Set.member` s || 9 `Set.member` s
has' 9 s = 6 `Set.member` s || 9 `Set.member` s
has' x s = x `Set.member` s 

needs = [ (0,1)
        , (0,4)
        , (0,9)
        , (1,6)
        , (2,5)
        , (3,6)
        , (4,9)
        , (6,4)
        , (8,1)]