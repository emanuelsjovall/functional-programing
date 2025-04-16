import Debug.Trace (trace)
import Data.List

import Data.Tree

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = 
    fibonacci (n-2) + fibonacci (n-1)

funCache :: (Int -> Int) -> [Int]
funCache f = map f [0..]

doubleCache = funCache (*2)

cacheLookup :: [Int] -> Int -> Int
cacheLookup cache i = cache !! i

fibonacciCache :: [Int]
fibonacciCache = funCache fastFibonacci

fastFibonacci :: Int -> Int
fastFibonacci 0 = 0
fastFibonacci 1 = 1
fastFibonacci n = 
    let fetch = cacheLookup fibonacciCache
    in fetch (n-2) + fetch(n-1)


removeHead :: [a] -> [a]
removeHead = drop 1

removeLast :: [a] -> [a]
removeLast l = take (length l - 1) l

subStrings :: String -> [String]
subStrings [] = []
subStrings [x] = [[], [x]]
subStrings s =
    let children = [removeHead s, removeLast s]
        subStrings1 = map subStrings children
    in nub $ concat subStrings1 ++ [s]

trieCache :: (String -> b) -> Trie b Char
trieCache f = mapTrie f $ rootTrie ['a'..'z']

subStringsCache :: Trie [String] Char
subStringsCache = trieCache fastSubStrings

fastSubStrings :: String -> [String]
fastSubStrings [] = []
fastSubStrings [x] = [[], [x]]
fastSubStrings s = 
    let children = [removeHead s, removeLast s]
        fetch s = lookupTrie S fastSubStrings
        subStrings1 = map fetch children
    in nub $ concat subStrings1 ++ [s]