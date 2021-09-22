module Main where

main = putStrLn "Hello"

infixr 4 `choose`
infixr 4 `mc`

n `choose` r = fact n `div` (fact r * fact (n - r))
fact n = product [1..n]
n `mc` r = n + r - 1 `choose` r
n `permutations` r = fact n `div` fact (n - r)