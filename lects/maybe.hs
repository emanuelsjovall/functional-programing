import Prelude hiding (Monad, return, (>>=))

people = [("Alice", 1), ("Bob", 2)]
taxOwed = [(1, 1200), (3, -5400)]

unwrap :: Maybe a -> a
unwrap (Just x) = x

-- findTax :: String -> Maybe Integer
-- findTax p
--     | identifier == Nothing = Nothing
--     | otherwise = lookup (unwrap identifier) taxOwed
--     where
--         identifier = lookup p people

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) f = f x

-- findTax :: String -> Maybe Integer
-- findTax p = bind (lookup p people) (\i -> lookup i taxOwed)

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
    return x = Just x
    (>>=) = bind

-- findTax :: String -> Maybe Integer
-- findTax p = lookup p people >>= (\i -> lookup i taxOwed)

findTax :: String -> Maybe Integer
findTax p = do
    i <- lookup p people
    owed <- lookup i taxOwed
    return owed
findTax p = lookup p people >>= (\i -> lookup i taxOwed >>= (\owed -> Just owed)) 