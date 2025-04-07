module MatrixMap where

    import Data.Map
    
    type Position = (Int, Int)

    type Matrix a = Map Position a

    emptyMatrix :: Matrix a
    emptyMatrix = empty

    get :: Matrix a -> Position -> Maybe a
    get m (x, y) = m !? (x, y)

    set :: Matrix a -> Position -> a -> Matrix a
    set m (x, y) value = insert (x, y) value m

    example :: Maybe String
    example =
        let matrixWithX = set emptyMatrix (1, 1) "X"
        in get matrixWithX (0, 0)

    extractValue :: Maybe a -> a -> a
    extractValue Nothing def = def
    extractValue (Just x) def = x