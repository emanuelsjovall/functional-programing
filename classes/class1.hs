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
    numFactors n = length $ removeDuplicateshelper $ map (flip nextFactor n) [1..n]

    removeDuplicateshelper :: Eq a => [a] -> [a]
    removeDuplicateshelper [] = []
    removeDuplicateshelper [x] = [x]
    removeDuplicateshelper (x:xs)
        | elem x xs = removeDuplicateshelper xs
        | otherwise = x : removeDuplicateshelper xs


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
    validDate (Date year month day)
        | day > 0 && day <= daysInMonth month year = True
        | otherwise = False

    -- Define a function
    --   multiply :: Num a => [a] -> a
    -- which multiplies together all the elements of a list. (Think: what should its
    -- value be for the empty list?). For example
    -- Main> multiply [1,2,3,4,5]
    -- 120
    -- (This is actually a standard function, called product).
    multiplyf :: Num a => [a] -> a
    multiplyf = foldr (*) 1

    multiplyr :: Num a => [a] -> a
    multiplyr [] = 1
    multiplyr (x:xs) = x * multiplyr xs

    -- Define a function substituting elements in a list by another element. E.g.
    -- Main> substitute ’e’ ’i’ "eigenvalue"
    -- "iiginvalui"
    substitutem :: Eq a => a -> a -> [a] -> [a]
    substitutem from to = map (\x -> if x == from then to else x)

    substituter :: Eq a => a -> a -> [a] -> [a]
    substituter _ _ [] = []
    substituter from to (x:xs) = substituted : substituter from to xs
        where substituted = if x == from then to else x


    -- 2.2.3 Avoiding duplicates (TDA555)
    -- In many situations, lists should not contain duplicate elements. For example, a
    -- pack of cards should not contain the same card twice. Define a function
    --     duplicates :: Eq a => [a] -> Bool
    -- which returns True if its argument contains duplicate elements.
    -- Main> duplicates [1,2,3,4,5]
    -- False
    -- Main> duplicates [1,2,3,2]
    -- True
    -- Hint: the standard function elem, which tests whether an element occurs in a
    -- list, is helpful here.

    --     One way to ensure a list contains no duplicates is to start with a list that
    -- might contain duplicate elements, and remove them. Define a function
    --     removeDuplicates :: Eq a => [a] -> [a]
    -- which returns a list containing the same elements as its argument, but without
    -- duplicates. Test it using the following property:
    --     prop_duplicatesRemoved :: [Integer] -> Bool
    --     prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))
    -- Does this property guarantee that removeDuplicates behaves correctly? If not,
    -- what is missing?
    -- (removeDuplicates is actually a standard function, called nub)
    duplicates :: Eq a => [a] -> Bool
    duplicates [] = False
    duplicates (x:xs) = elem x xs || duplicates xs

    -- The above one called removeDuplicatesHelper is probably better this is just for practice
    removeDuplicates :: Eq a => [a] -> [a]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = [x | notElem x xs] ++ removeDuplicates xs
    -- | elem x xs = removeDuplicates xs
    -- | otherwise = x : removeDuplicates xs

    prop_duplicatesRemoved :: Eq a => [a] -> Bool
    prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))
    -- No since if xs = [] it will return True even if remove might be wrong

    -- 2.2.4 Comprehensions (TDA555)
    -- Describe what does the function
    -- pairs :: [a] -> [b] -> [(a,b)]
    -- pairs xs ys = [(x,y) | x<-xs, y<-ys]
    -- Takes two lists of elements and zips them together and creates list of tuples
    -- do.
    --     A Pythagorean triad is a triple of integers (a,b,c) such that
    -- a^2 + b^2 == c^2
    -- Define a function of n that will find all Pythagorean triads with a<=b<=c<=n
    pythagoreanTriads :: Integer -> [(Integer, Integer, Integer)]
    pythagoreanTriads n = [(a, b, c) | a <- [1 .. n], b <- [a .. n], c <- [b .. n], a ^ 2 + b ^ 2 == c ^ 2]

    -- 2.2.5 Permutations (TDA555)
    -- A permutation of a list is another list with the same elements, but in a possibly
    -- different order. For example, [1,2,1] is a permutation of [2,1,1], but not of
    -- [1,2,2]. Write a function
    --     isPermutation :: Eq a => [a] -> [a] -> Bool
    -- that returns True if its arguments are permutations of each other.
    isPermutation :: Eq a => [a] -> [a] -> Bool
    isPermutation [] [] = True
    isPermutation [] _ = False
    isPermutation _ [] = False
    isPermutation (x:xs) ys
        | elem x ys = isPermutation xs (remove x ys)
        | otherwise = False

    remove :: Eq a => a -> [a] -> [a]
    remove _ [] = []
    remove e (x:xs)
        | e == x = xs
        | otherwise = x : remove e xs

    quickSort :: Ord a => [a] -> [a]
    quickSort []     = []
    quickSort (p:xs) = quickSort lesser ++ [p] ++ quickSort greater
        where
            lesser  = filter (< p) xs
            greater = filter (>= p) xs
    
    -- 2.2.6 Shortest and Longest (Chakravarty)
    -- Determine the shortest and the longest string in a list. E.g.:
    -- Main> shortestAndLongest ["abc"]
    -- ("abc","abc")
    -- Main> shortestAndLongest ["This", "sentence", "is","ridiculous"]
    -- ("is","ridiculous")
    shortestAndLongest :: [String] -> (String, String)
    shortestAndLongest [] = ([], [])
    shortestAndLongest xs = (theShortest xs, theLongest xs)

    theShortest :: [String] -> String
    theShortest (x:xs)
        | null xs || shorter x xs = x
        | otherwise = theShortest xs

    shorter :: String -> [String] -> Bool
    shorter x xs = length x <= minimum (map length xs)
    -- shorter x xs = length x <= (minimum $ map length xs)

    theLongest :: [String] -> String
    theLongest (x:xs)
        | null xs || longest x xs = x
        | otherwise = theLongest xs
    
    longest :: String -> [String] -> Bool
    longest x xs = length x >= maximum (map length xs)
    -- longest x xs = length x >= (maximum $ map length xs)

    -- 2.2.7 Mystery (Thompson)
    -- What does the following function do?
    -- mystery xs = foldr (++) [] (map (\y -> [y]) xs)
    -- Nothing takes a list turns every item in it to a list with only itself then concatinates all of the lists
    -- back into one list aka [1, 2, 3] map -> [[1], [2], [3]] foldr -> [1, 2, 3]
