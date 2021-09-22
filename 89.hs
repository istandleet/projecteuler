import Data.Char
import Data.Maybe
import System.Environment

main = getArgs >>= readFile . head >>= print . sum . map diffLine . lines

diffLine :: String -> Int
diffLine l = length l - length (toRCs $ fromStringRC l)

data RC = I | V | X | L | C | D | M deriving (Eq, Ord, Show)
fromChar :: Char -> RC
fromChar = fromJust . flip lookup [('M',M),('D',D),('C',C),('L',L),('X',X),('V',V),('I',I)]
rc M = 1000
rc D = 500 
rc C = 100 
rc L = 50  
rc X = 10  
rc V = 5   
rc I = 1   

toRCs :: Int -> [RC]
toRCs n | n <= 0    = []
        | n <  4    = I :     toRCs (n - 1)
        | n == 4    = I : V : toRCs (n - 4)
        | n <  9    = V :     toRCs (n - 5)
        | n == 9    = I : X : toRCs (n - 9)
        | n <  40   = X :     toRCs (n - 10)
        | n <  50   = X : L : toRCs (n - 40)
        | n <  90   = L :     toRCs (n - 50)
        | n <  100  = X : C : toRCs (n - 90)
        | n <  400  = C :     toRCs (n - 100)
        | n <  500  = C : D : toRCs (n - 400)
        | n <  900  = D :     toRCs (n - 500)
        | n <  1000 = C : M : toRCs (n - 900)
        | otherwise = M :     toRCs (n - 1000)

fromStringRC :: String -> Int
fromStringRC = fin . foldr stepRC (0,Nothing) . map fromChar
    where
      fin (n,mc) = n + maybe 0 rc mc
      stepRC c (n,Nothing) = (n,Just c)
      stepRC c (n,Just pc) = if c >= pc then (n + rc pc,Just c)
                            else (n + rc pc - rc c,Nothing)