import Prelude hiding (Functor, fmap, pure, (<*>), Applicative)

list1 = [1, 2, 3, 4, 5]
list2 = [6, 7, 8, 9, 10]
list3 = [11, 12, 13, 14, 15]

add1 :: Num a => [a] -> [a]
-- add1 [] = []
-- add1 (x:xs) = (x+1) : add1 xs
add1 = map (+1)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

add1_general :: (Functor f, Num a) => f a -> f a
add1_general = fmap (+1)

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- mul2 :: (Functor f, Num a) => f a -> f a

plus3 :: Num a => a -> a -> a -> a
plus3 x y z = x + y + z