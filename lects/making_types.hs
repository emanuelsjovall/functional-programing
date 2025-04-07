type MyCoolType = String

x :: MyCoolType
x = "Hello"

-- Can only have one "Primitive" type over
newtype MyCoolerType = CoolerType String
    deriving (Show)

myreverse (CoolerType s) = CoolerType (reverse s)


-- Kinda lika class from java
data Season = Spring | Summer | Autumn | Winter
    deriving (Show, Enum)

data SeasonT t = SpringT t | SummerT t | AutumnT t | WinterT t
    deriving (Show)

data Optional a = EmptyOptional | ItemOptional { thing :: a }
    deriving (Show)

data Tree a = EmptyNode | Node (Tree a) a (Tree a)
    deriving (Show)

insert :: Ord a => Tree a -> a -> Tree a
insert EmptyNode x = Node EmptyNode x EmptyNode
insert (Node left val right) x
    | x < val = Node (insert left x) val right
    | x > val = Node left val (insert right x)
    | otherwise = Node left val right

contains :: Ord a => Tree a -> a -> Bool
contains EmptyNode _ = False
contains (Node left val right) x
    | x < val = contains left x
    | x > val = contains right x
    | otherwise = True


-- Typeclass kinda like interface from java
class Set t where
    class_insert :: Ord a => t a -> a -> t a
    class_contains :: Ord a => t a -> a -> Bool

instance Set Tree where 
    class_insert = insert
    class_contains = contains