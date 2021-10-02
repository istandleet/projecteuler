{-
[incomplete]

We start with the kelly strategy and bet 20%.

We zero out anything that can't possibly win anymore

Without any "I am getting desparate" or "I should bet less than 20%, I am so close",
we get a probability of 0.13907185020 (which is not the answer). 

Using these naive strategies you end up getting

(1 % 10,"0.00002711300")
(1 % 5,"0.13907185020")
(3 % 10,"0.12174833600")
(2 % 5,"0.02205841390")
(1 % 2,"0.00093014970")
(3 % 5,"0.00002281220")
(7 % 10,"0.00000114840")

Zooming in, we see

(17 % 100,"0.07871452700")
(9 % 50,"0.10135014040")
(19 % 100,"0.12211120360")
(1 % 5,"0.13907185020")
(21 % 100,"0.15186426620")
(11 % 50,"0.15918496150")
(23 % 100,"0.16499549610")
(6 % 25,"0.16631307750") -- 0.24
(1 % 4,"0.16418439240")

Adding in "selling out" on turns 999 and 1000 got us up to 0.17366783570

In a perfect world we get to exactly 1 trillion. 
Once you get above 1-1/2^tr, you should be betting to end up at exactly 
1 trillion every move (equivalently, you should bet to end up at exactly 1-1/2^tr)

Adding that in didn't move the number

search space peaks near 400 paths at 700 rounds

Adding full desperation throughout got to 0.18302930670 (91s)

Hardcoding strat on turn 3 got to 0.18584164955 (80s)
-}

{-# language BangPatterns #-}
module Main where

import Control.Monad
import Control.Parallel.Strategies
import Data.Foldable
import Data.Scientific
import qualified Data.Map.Strict as Map
import Data.Map (Map)

--main = putStrLn $ pprintRat answer
--main = mapM_ print $ zip ds $ map pprintRat $ parMap rdeepseq (\d -> answer' $ \_ _ -> d) ds
--    where ds = [0.17,0.18..0.25]
main = go $ zip [0..] $ turns wager
    where
    go [d] = putStrLn $ pprintRat $ getAnswer $ snd d
    go ((i,d):ds) = do
        when (i `mod` 25 == 0) $ print (i,length d, sum d)
        go ds

pprintRat = formatScientific Fixed (Just 11) 
          . either fst fst . fromRationalRepetend (Just 15)
     
answer :: Rational
answer = getAnswer $ answer' wager 1000 1

getAnswer = (Map.! _TRILLION)

answer' :: WagerFunction -> TurnsRemaining -> Bankroll -> Dist
answer' wf turns initial = foldl' (\x f -> f x) start $ map (step wf) [turns,pred turns..1]
    where start = Map.singleton initial 1

type Dist = Map Bankroll Rational

turns wf = scanl (\x f -> f x) start $ map (step wf) [1000,999..1]

start :: Dist
start = Map.singleton 1 1

type TurnsRemaining = Int
type Bankroll = Rational
type Wager = Rational -- % of bankroll (0-1)
type WagerFunction = TurnsRemaining -> Bankroll -> Wager

step :: WagerFunction -> TurnsRemaining -> Dist -> Dist
step f turnsremaining = Map.fromListWith (+) . Map.foldMapWithKey go
    where 
    go bankroll p 
        | bankroll >= _TRILLION = [(_TRILLION,p)]
        | bankroll < min_to_win = [(0,p)]
        -- | bankroll < 2*min_to_win = [(2*min_to_win,0.6*p),(0,0.4*p)]
        | otherwise = bet bankroll p $ f turnsremaining bankroll
    bet bankroll p 0 = [(bankroll,p)]
    bet bankroll p b = [
            (min _TRILLION $ bankroll+b*bankroll,p*0.6),
            (bankroll-b*bankroll,0.4*p)]
    !min_to_win = _TRILLION / 2^turnsremaining


wager :: WagerFunction
wager _ bankroll | bankroll > _TRILLION = 0
-- Once you get above 1-1/2^tr, you can try to win each move
wager tr br | br >= highline = target (1-(recip$2^(tr-1))) br
    where highline = pTrillion $ 1 - (recip$2^tr)
wager tr br | br <= lowline = target (recip$2^(tr-1)) br
    where lowline = pTrillion (recip$2^(tr-1))
-- last turn go all in
wager 1 _ = 1
-- with two turns left go to 50%. if you're above 75 you might win.
wager 2 br = target 0.5 br
wager 3 br
    | br > pTrillion 0.875 = error "should have highlined 3"
    | br > pTrillion 0.5 = target 0.75 br
    | otherwise = target 0.25 br
wager turnsremaining bankroll = 0.24 -- from experimentation

pWinning _ ptril | ptril >= 1 = 1
pWinning tr br | br >= highline = 1 - 0.4^tr
    where highline = 1 - (recip$2^tr)
pWinning tr br | br < lowline = 0
    where lowline = recip$2^tr
pWinning tr br | br < lowline = 0.6^tr
    where lowline = recip$2^(tr-1)
pWinning 2 br = 0.6 -- unless above 0.75 or below 0.5
pWinning 3 br
    --| br > 0.875 = error "should have highlined 3"
    --| br < 0.25 = error "should have been lowlined 3"
    | br >= 0.5 = 0.6 * pWinning 2 0.75 + 0.4 * pWinning 2 0.25
    | otherwise = pWinning 2 0.25
pWinning 4 br
    --| br > 0.9375 = error "should have highlined 4"
    --| br < 0.125 = error "should have been lowlined 4"
    | br >= 0.5 = 0.6 * pWinning 2 0.75 + 0.4 * pWinning 2 0.25
    | otherwise = pWinning 2 0.25
pWinning turnsremaining ptril = 0

target r br = min 1 $ abs $ 1 - pTrillion r / br

_TRILLION :: Bankroll
_TRILLION = 10^12
pTrillion r = r * _TRILLION