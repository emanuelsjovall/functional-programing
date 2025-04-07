module MatrixList where

    type Matrix a = [[a]]

    emptyMatrix :: Int -> Int -> a -> Matrix a
    emptyMatrix r c empty = [emptyRow | i <- [1..r]]
        where emptyRow = [empty | j <- [1..c]]

    get :: Matrix a -> Int -> Int -> a
    get m r c = (m !! r) !! c

    set :: Matrix a -> Int -> Int -> a -> Matrix a
    set m r c v =
        let newRow row = take c row ++ [v] ++ drop (c + 1) row
            selectedRow = m !! r
            newMat mat = take r mat ++ [newRow selectedRow] ++ drop (r + 1) mat
        in newMat m

    example :: Bool
    example =
        let start = emptyMatrix 3 3 "-"
            step1M = set start 0 0 "X"
        in get step1M 0 0 == "X"
