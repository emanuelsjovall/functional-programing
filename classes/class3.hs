module Class3 where


-- 2.1 Simple definitions (Thompson)
-- Show that binary trees can be given a monad structure. Show the same for the type

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Leaf v) = Leaf (f v)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Applicative Tree where
    pure = Leaf
    (Leaf f) <*> v = fmap f v
    (Node l r) <*> v = Node (l <*> v) (r <*> v)

instance Monad Tree where
   -- return = Leaf
   (Leaf t) >>= f   = f t
   (Node l r) >>= f = Node (l >>= f) (r >>= f)


data Error a = Ok a | Error String

instance Functor Error where
    fmap f (Ok v) = Ok (f v)
    fmap f (Error s) = Error s

instance Applicative Error where
    pure = Ok
    (Ok f) <*> v = fmap f v
    (Error s) <*> v = Error s

instance Monad Error where
    return = Ok
    (>>=) :: Error a -> (a -> Error b) -> Error b
    (Ok v) >>= f = f v
    (Error s) >>= f = Error s

-- 2.2 Monadic helper functions (TDA555)
-- Give an implementation of the following two functions:
-- sequence_ :: Monad m => [m ()] -> m ()
-- onlyIf :: Monad m => Bool -> m () -> m ()
-- sequence takes a list of instructions resulting in an uninteresting value, and
-- creates one big instruction that executes all of these.
-- onlyIf takes a boolean and an instruction, and creates an instruction that
-- only executes the argument instruction if the boolean was True. If the boolean
-- was False, nothing happens. Example onlyIf failed tryAgain executes the
-- instructions tryAgain only if the boolean failed is True.
-- Hint: You might find it easier to think of the above functions having type:
-- sequence_ :: [IO ()] -> IO ()
-- onlyIf :: Bool -> IO () -> IO ()

sequence2_ :: Monad m => [m ()] -> m ()
sequence2_ = foldr (>>) (return ())

sequence3_ :: Monad m => [m ()] -> m ()
sequence3_ [] = return () 
sequence3_ (x:xs) = do 
    x
    sequence3_ xs

onlyIf :: Monad m => Bool -> m () -> m ()
-- onlyIf b f
--     | b = f
--     | otherwise = return ()
onlyIf true f = f
onlyIf false _ = return ()


-- What becomes different if we change the type of onlyIf to:
-- onlyIfM :: Monad m => m Bool -> m () -> m ()
-- What other kinds of programs can we write now?
-- Give an implementation of onlyIfM.

onlyIfM :: Monad m => m Bool -> m () -> m ()
-- onlyIfM b f = b >>= (\x -> if x then f else return ())
onlyIfM b f = do
    result <- b
    if result then f else return ()


-- 2.3 List comprehension (lecture)
-- Let us take the example from the lecture:
list1 = [ (x,y) | x<-[1..], y<-[1..x]]
list2 = do
    x <- [1..]
    y <- [1..x]
    return (x,y)
-- Rewrite list2 using bind (>>=) instead of do.
list3 = [1..] >>= (\x -> [1..x] >>= (\y -> return (x,y)))


-- 2.4 Some theorem proving (Thompson) (this ones a skip for now lmao)
-- First, let us remind that the full Monad class is defined as follows:
-- class Monad m where
--    (>>=) :: m a -> (a -> m b) -> m b
--    return :: a -> m a
--    (>>) :: m a -> m b -> m b
--    fail :: String -> m a
-- where (>>) and fail may be defined as defaults:
-- m >> k = m >>= \_ -> k
-- fail s = error s
-- So >> just discards the value of the first expression, whereas >>= passes it to
-- the next one.
-- Let us now define yet another derived operator, >@>:
-- (>@>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- f >@> g = \x -> (f x) >>= g
-- This is the Kleisli composition.

-- Let us now formulate three requirements on the operations of a monad.
-- First, return acts as identity for the operator >@>:
-- return >@> f = f (M1)
-- f >@> return = f (M2)
-- and the operator >@> should be associative:
-- (f >@> g) >@> h = f >@> (g >@> h) (M3)
-- Task 1: For the monads Id, [] and Maybe prove the rules (M1) to (M3). Nah im good

-- instance Monad Id where
--     return x = Id x
--     (Id x) >>= f = f x

-- (return x) >>= f === (Id x) >>= f === f x
-- m >>= return === (Id x) >>= return === return x === (Id x) === m
-- LHS3: (m >>= f) >>= g === ((Id x) >>= f ) >>= g === (f x) >>= g 
-- RHS3: m >>= \x -> (f x >>= g) === (Id x) >>= \x -> (f x >>= g) === (f x >>= g)


-- Let us define some standard functions over every monad:
-- fmap :: Monad m => (a -> b) -> m a -> m b
-- join :: Monad m => m (m a) -> m a
-- fmap f m = do
--            x <- m
--            return (f x)
-- join m = do
--          x <- m
--          x
-- Over the lists they correspond to map and concat, respectively. Now we can
-- formulate the following dependency:
-- fmap (f . g) = fmap f . fmap g (M4)
-- Task 2: Prove the property (M4) using the laws (M1) to (M3).


-- Task 3: Prove the following properties using the monad laws:
-- join return = join . fmap return
-- join return = id


-- Task 4: Write down the definitions of map and join over lists using list comprehensions. Compare them with the definitions of fmap and join given in the do-notation above.


-- 2.5 State Monad (RealWorldHaskell)
-- You may wish to have a look at Chapter 14 of “Real World Haskell” where,
-- among others, the State monad is gently introduced. In particular, the following
-- set of definitions come there:
newtype State s a = State {
    runState :: s -> (a, s)
}

returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> 
    let (a, s') = runState m s 
    in runState (k a) s'

instance Functor (State s) where


instance Applicative (State s) where


instance Monad (State s) where
    return a = returnState a
    m >>= k = bindState m k

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- Task 1: (and only one:-) Rewrite the random number generator as an instance
-- of the state monad, using the functions put and get.
-- The StateTransform monad from the lecture notes is actually equivalent to
-- the above State monad, with possibly a slightly different set of help functions
-- defined, in particular:

updateST :: (s -> s) -> State s ()
updateST u = State $ \s -> ((), u s)

queryST :: (s -> a) -> State s a
queryST q = State $ \s -> (q s, s)

-- Finally, please note that the StateTransform monad is not a monad transformer (as defined in Chapter 18 of RWH).

-- will give errors since System.Random is not instaled by default on my mac and I just can not be bothered to install it
-- type RandomState a = State StdGen a

-- getRandom :: Random a => RandomState a
-- getRandom = get >>= \gen -> 
--     let (val, gen') = random gen 
--     in put get' >> return val

-- getRandomDo :: Random a => RandomState a
-- getRandomDo = do
--     gen <- get
--     let (val, gen') = random gen
--     put gen'
--     return val
