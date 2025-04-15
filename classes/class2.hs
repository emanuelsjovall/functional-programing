
module Class2 where
import Data.Maybe
import Data.List
import Text.Read (Lexeme(String))

-- 1. Design a data type Proposition to represent propositions.
data Proposition = Var String
                | Proposition :&: Proposition
                | Proposition :|: Proposition
                | Not Proposition
    deriving ( Eq, Show )

-- 2. Define a function
-- vars :: Proposition -> [String]
-- which returns a list of the variables in a proposition. Make sure each
-- variable appears only once in the list you return.
vars :: Proposition -> [String]
vars (Var s) = [s]
vars (a :&: b) = union (vars a) (vars b)
vars (a :|: b) = union (vars a) (vars b)
vars (Not a) = vars a

-- Suppose you are given a list of variable names and their values, of type
-- Bool, for example, [("p", True), ("q", False)]. Define a function
-- truthValue :: Proposition -> [(String, Bool)] -> Bool
-- which determines whether the proposition is true when the variables have
-- the values given.
truthValue :: [(String, Bool)] -> Proposition -> Bool
truthValue val (Var x) = fromJust (lookup x val)
truthValue val (a :&: b) = truthValue val a && truthValue val b
truthValue val (a :|: b) = truthValue val a || truthValue val b
truthValue val (Not a) = truthValue val a


-- 3. Define a function
-- tautology :: Proposition -> Bool
-- which returns true if the proposition holds for all values of the variables
-- appearing in it.
tautology :: Proposition -> Bool
tautology p = and [ truthValue vals p | vals <- allVals (vars p)]

-- allVals xs enumerates all possible valuations of the variables xs:
--    1. when xs = [], there is just one valuation
--    2. otherwise, we enumerate all possible valuations for the rest
--       of the variables, plus all possible values of x
allVals :: [String] -> [[(String, Bool)]]
allVals [] = [[]]
allVals (x:xs) = [(x, b):vals | vals <- allVals xs, b <- [True, False]]

-- 2.2 File Systems (TDA555)
-- A file either contains data or is a directory. A directory contains other files
-- (which may themselves be directories) along with a name for each one.
-- 1. Design a data type to represent the contents of a directory. Ignore the
-- contents of files: you are just trying to represent file names and the way
-- they are organised into directories here.
data File = File String
        | Dir String [File]
    deriving (Show, Eq)

-- 2. Define a function to search for a given file name in a directory. You should
-- return a path leading to a file with the given name. Thus if your directory
-- contains a, b, and c, and b is a directory containing x and y, then searching
-- for x should produce b/x.
search :: [File] -> String -> [String]
search files name = 
    [ name | File name' <- files, name == name']
    ++
    [ dir ++ "/" ++ path | Dir dir files' <- files, path <- search files' name]

searchMaybe :: [File] -> String -> Maybe String
searchMaybe files name = 
    listToMaybe (
        [ name | File name' <- files, name == name'] 
        ++
        [ dir ++ "/" ++ path | Dir dir files' <- files, Just path <- [searchMaybe files' name]]
    )

-- it can also be defined using the first one...
searchMaybe' :: [File] -> String -> Maybe String
searchMaybe' files name = listToMaybe (search files name)

exampleFileSystem :: [File]
exampleFileSystem =
  [ File "apa"
  , Dir "bepa" [ File "apa", Dir "bepa" [], Dir "cepa" [ File "bepa" ] ]
  , Dir "cepa" [ Dir "bepa" [], Dir "cepa" [ File "apa" ] ]
  , Dir "shit" [ Dir "poop" [ File "pee" ] ]
  ]


-- 2.3 Sets (TDA555)
-- 1. Design a datastructure for sets . I.e. there should be a type Set a, and a
-- number of functions for creating, combining, and investigating sets. There
-- should at least be a function to create an empty set, add an element to a
-- set, take the union of two sets, remove an element from the set, and check
-- if an element is in the set.
-- 2. Now, implement the Set datastructure. You may use lists internally.
data Set a = Set [a]
    deriving (Show)

empty :: Set a
empty = Set []

add :: Eq a => a -> Set a -> Set a
--add item (Set list) = if elem item list then Set list else Set (item : list)
add item (Set list)
    | elem item list = Set list
    | otherwise = Set (item : list)

remove :: Eq a => a -> Set a -> Set a
remove item (Set list) = Set [x | x <- list, x /= item]

sunion :: Eq a => Set a -> Set a -> Set a
sunion (Set l1) (Set l2) = Set ([x | x <- l2, notElem x other] ++ other)
    where other = l1

contains :: Eq a => a -> Set a -> Bool
contains item (Set s) = elem item s


-- 3. Redo the above exercise, but now use sorted lists of unique elements as
-- your internal representation. Set union becomes more efficient that way.

-- 2.4 Ordering (Thompson)
-- Complete the following instance declarations:
-- instance (Ord a, Ord b) => Ord (a,b) where ...
-- instance Ord b => Ord [b] where ...
-- where pairs and lists should be ordered lexicographically, like the words in dictionary
-- ordering

-- note that you have to comment this section out to get the file type-checked,
-- as those two conflict with Data.Tuple and GHC.Base, respectively
-- instance (Ord a, Ord b) => Ord (a,b) where 
--   (x,y) < (z,w)       =  x<z || x==z && y<w
--   (x,y) <= (z,w)      =  x<z || x==z && y<=w
--   (x,y) > (z,w)       =  x>z || x==z && y>w 
--   (x,y) >= (z,w)      =  x>z || x==z && y>=w 
--   max (x,y) (z,w)
--                     | (x,y) >= (z,w) = (x,y)
--                     | otherwise      = (z,w)
--   min (x,y) (z,w) 
--                     | (x,y) <= (z,w) = (x,y)
--                     | otherwise      = (z,w)
--   (x,y) `compare` (z,w)
--                     | (x,y) == (z,w) = EQ
--                     | (x,y) < (z,w)  = LT
--                     | (x,y) > (z,w)  = GT


-- instance Ord b => Ord [b] where 
--   [] < _                  = True
--   _  < []                 = False
--   (x:xs) < (y:ys)       = x < y || x==y && xs < ys
--   x <= y      = x < y || x == y
--   x > y      = y < x
--   x >= y     = y <= x
--   x `max` y    
--              | x >= y = x
--              | otherwise = y
--   x `min` y    
--              | x >= y = y
--              | otherwise = x
--   x `compare` y
--              | x == y = EQ
--              | x > y = GT
--              | x < y = LT


-- 2.5 ListNatural (lecture)
-- Natural numbers may correspond to lists of nothing!!
-- type ListNatural = [()]
-- For example:
-- twoL = [(),()]
-- threeL = [(),(),()]
-- What is: (:) is +1
-- What is: (++) is add
-- What is: map (const ()) is toListNatural (coerce) replaces anything with () map (const ()) [1, 2, 3] -> [(), (), ()]

-- 1. What do these functions do?
-- f1 x y = foldr (:) x y is add
-- f2 x y = foldr (const (f1 x)) [] y is multiply
-- f3 x y = foldr (const (f2 x)) [()] y is power

-- 2. Continue this definition:
-- instance Num ListNatural where ...

-- Note: This requires ListNatural to be declared as a newtype1. 
-- One can ask: Why?
newtype ListNatural = LN [()] deriving (Show, Eq) 

zeroJM = LN []
zeropJM (LN x) = x == []
plusJM (LN x) (LN y) = LN (x++y)
incJM (LN x) = LN (():x)
decJM (LN x) 
   | zeropJM (LN x) = error "no negative naturals exist"
   | otherwise = LN (tail x)
minusJM (LN x) (LN y) 
   | length x < length y = error "no negative naturals exist"
   | zeropJM (LN y) = LN x
   | otherwise = minusJM (decJM (LN x)) (decJM (LN y)) 
timesJM (LN x) (LN y) = LN (concatMap (const x) y)
absJM (LN x) = LN x
signumJM (LN x) 
   | zeropJM (LN x) = zeroJM
   | otherwise = incJM zeroJM
toIntegerJM (LN x) = length x
fromIntegerJM x 
   | x < 0  = error "no negative naturals exist"
   | x == 0 = zeroJM
   | otherwise = LN (toList x) where
        toList 0 = []
        toList n = () : toList (n-1)

instance Num ListNatural where
   (+) = plusJM
   (*) = timesJM
   (-) = minusJM
   abs = absJM
   signum = signumJM
   negate = error "no negative naturals exist"
   fromInteger = fromIntegerJM


-- 2.6 Type derivation
-- Give the types of the following expressions:
-- 1. (.)(:) -> (a -> b) -> a -> [b] -> [b]
-- 2. (:(.)) -> Invalid
-- 3. ((.):) -> [(b -> c) -> (a -> b) -> a -> c] -> [(b -> c) -> (a -> b) -> a -> c]
-- 4. ((:):) -> [a -> [a] -> [a]] -> [a -> [a] -> [a]]
-- 5. Haskel wheels: (.)(.) -> (a1 -> b -> c) -> a1 -> (a2 -> b) -> a2 -> c
-- 6. The Haskell smiley: (8-) -> Num a => a -> a
-- 7. Haskell goggles: (+0).(0+) -> Num c => c -> c
-- 8. A Haskell treasure: (($)$($)) -> (a -> b) -> a -> b
-- 9. Haskell swearing: ([]>>=)(\_->[(>=)]) -> Ord a => [a -> a -> Bool]