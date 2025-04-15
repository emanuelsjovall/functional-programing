x = 2*2*2*3*3*5*5*7*7*13*17*23
x2 = 1117*2417*3853*4793*6971*7919

isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = True
isPrime (p:ps) x
    | mod x p == 0 = False
    | p*p > x = True
    | otherwise = isPrime ps x

nextPrime :: [Integer] -> (Integer, [Integer])
nextPrime [] = (2, [2])
nextPrime [2] = (3, [2, 3])
nextPrime primes = (p, primes ++ [p])
    where
        p = until (isPrime primes) (+2) (last primes + 2)

extractPrimeFactor :: Integer -> Integer -> (Int, Integer)
extractPrimeFactor p x = extractPfactor p 0 x
    where
        extractPfactor p n x
            | mod x p == 0 = extractPfactor p (n+1) (div x p)
            | otherwise = (n, x)

primeFactors :: Integer -> [Integer]
primeFactors x = pfactors [] x
    where
        pfactors _ 1 = []
        pfactors primes x = replicate n p ++ pfactors primes' x' 
            where
                (p, primes') = nextPrime primes
                (n, x') = extractPrimeFactor p x


newtype Stateful a b = ST (a -> (b, a))

instance Functor (Stateful a) where
    fmap f (ST gen) = ST (\ps -> let (x, ps') = gen ps in (f x, ps'))

instance Applicative (Stateful a) where
    pure x = ST (\ps -> (x, ps))
    (ST f) <*> (ST gen) = ST (\ps -> let (x, ps') = f ps
                                         (y, ps'') = gen ps' in (x y, ps''))

instance Monad (Stateful a) where
    -- (ST gen) >>= f = ST (\ps -> let (x, ps') = gen ps
    --                                 (ST gen2) = f x in gen2 ps')
    (ST gen) >>= f = ST bind
        where
            bind ps = gen2 ps'
                where
                    (x, ps') = gen ps
                    (ST gen2) = f x

type PrimeGenerator a = Stateful [Integer] a

nextPrime2 :: PrimeGenerator Integer
nextPrime2 = ST np
    where 
        np [] = (2, [2])
        np [2] = (2, [2, 3])
        np ps = let p = until (isPrime ps) (+2) (last ps + 2) in (p, ps ++ [p])


run (ST f) state = f state

test = do
    a <- nextPrime2
    b <- nextPrime2
    c <- nextPrime2
    return (a + b + c)

test2 = nextPrime2 >>= (\a ->
        nextPrime2 >>= (\b ->
        nextPrime2 >>= (\c ->
        ST (\state -> ((a + b + c), state)))))

get (ST f) = fst (f [])

primeFactors2 :: Integer -> [Integer]
primeFactors2 x = get $ pfactors x
    where
        pfactors 1 = return []
        pfactors x = do
            p <- nextPrime2
            let (n, x') = extractPrimeFactor p x
            rest <- pfactors x'
            return ((replicate n p) ++ rest)