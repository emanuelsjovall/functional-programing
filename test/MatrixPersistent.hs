module MatrixPersistent where

    import Data.List

    type Position = (Int, Int)

    type Change a = (Position, a)

    type Matrix a = [Change a]

    emptyMatrix :: Matrix a
    emptyMatrix = []

    isPosition :: Position -> Change a -> Bool
    isPosition pos (pos1, _) = pos == pos1

    get :: Matrix a -> Position -> Maybe a
    get [] p = Nothing
    get history position =
        case find (isPosition position) history of
            Nothing -> Nothing
            Just (_, v) -> Just v

    set :: Matrix a -> Position -> a -> Matrix a
    set m p v =
        let newChange = (p, v)
        in newChange : m
    
    cancel :: Matrix a -> Matrix a
    cancel = drop 1