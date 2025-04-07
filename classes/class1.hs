module Class1 where

    maxi :: Ord a => a -> a -> a
    maxi x y = if x >= y then x else y


    sumsqr :: (Eq a, Num a) => a -> a
    sumsqr 0 = 0
    sumsqr n = sumsqr (n - 1) + n * n


    sumsqm :: (Num a, Enum a) => a -> a
    sumsqm n = sum (map (^2) [1..n])

    sumsqf :: (Num b, Enum b) => b -> b
    sumsqf n = foldl (\acc x -> acc + x^2) 0 [1..n]

    -- The Towers of Hanoi is an ancient puzzle, consisting of a collection of rings of
    -- different sizes, and three posts mounted on a base. At the beginning all the
    -- rings are on the left-most post as shown, and the goal is to move them all to the
    -- rightmost post, by moving one ring at a time from one post to another. But, at
    -- no time may a larger ring be placed on top of a smaller one!
    --      I         I    I
    --      I         I    I
    --     -I-        I    I
    --    --I--       I    I
    --   ---I---      I    I
    --  ----I----     I    I
    -- -----I-----    I    I
    -- ========================================
    -- Can you find a strategy for solving the puzzle based on recursion? That is, if
    -- you already know how to move n-1 rings from one post to another, can you find
    -- a way to move n rings?
    -- If you try out your strategy, you will quickly discover that quite a large
    -- number of moves are needed to solve the puzzle with, say, five rings. Can you
    -- define a Haskell function
    -- hanoi n
    hanoi :: (Eq t, Num t, Num a) => t -> a
    hanoi 1 = 1
    hanoi n = 2 * hanoi (n - 1) + 1

    -- A prime number p has only two factors, 1 and p itself. A composite number
    -- has more than two factors. Define a function
    -- smallestFactor n
    -- which returns the smallest factor of n larger than one. For example,
    -- smallestFactor 14 == 2
    -- smallestFactor 15 == 3
    -- Hint: write smallestFactor using an auxiliary function nextFactor k n which
    -- returns the smallest factor of n larger than k. You can define smallestFactor
    -- using nextFactor, and nextFactor by recursion.
    -- Now define
    -- numFactors n
    -- which computes the number of factors of n in the range 1..n, possibly except
    -- factor 1.

    nextFactor :: Integer -> Integer -> Integer
    nextFactor k n 
        | k >= n = n
        | mod n (k+1) == 0 = k+1
        | otherwise = nextFactor (k+1) n

    smallestFactor :: Integer -> Integer
    smallestFactor = nextFactor 1

    numFactors :: Integer -> Int
    numFactors n = length $ removeDuplicates $ map (flip nextFactor n) [1..n]

    removeDuplicates :: Eq a => [a] -> [a]
    removeDuplicates [] = []
    removeDuplicates [x] = [x]
    removeDuplicates (x:xs)
        | elem x xs = removeDuplicates xs
        | otherwise = x : removeDuplicates xs


    -- Define a data type Month to represent months, and a function
    -- daysInMonth :: Month -> Integer -> Integer
    -- which computes the number of days in a month, given also the year. (You can
    -- ignore leap centuries and the like: just assume that every fourth year is a leap
    -- year).
    -- Define a data type Date, containing a year, month, and day, and a function
    -- validDate :: Date -> Bool
    -- that returns True if the day in the date lies between 1 and the number of days
    -- in the month.

    type Month = Int

    daysInMonth :: Month -> Integer -> Integer
    daysInMonth m y
        | m <= 0 || m > 12 = 0
        | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12  = 31
        | m == 4 || m == 6 || m == 9 || m == 11 = 30
        | mod y 4 == 0 = 29
        | otherwise = 28

    data Date = Date Integer Month Integer

    validDate :: Date -> Bool
    validDate year month day
        let days = daysInMonth month year
        in days < 31 