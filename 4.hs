type Matrix = [[Int]]


main :: IO ()
main = do
    content <- readFile "4_test.txt"
    let ls = lines content
        mat = map (map toBinary) ls :: Matrix
    print (allRolls mat)


toBinary :: Char -> Int
toBinary c =
    case c of
        '.' -> 1
        '@' -> 0
        _   -> error "Invalid character"


edgeCase :: Matrix -> Int -> Int -> Int
edgeCase [] _ _ = 1
edgeCase m@(row0: _) row col
    | row < 0           = 1
    | row >= numRows    = 1
    | col < 0           = 1
    | col >= numCols    = 1
    | otherwise         = m !! row !! col
    where
        numRows = length m
        numCols = length row0

neighbors3x3 :: Matrix -> Int -> Int -> [Int]
neighbors3x3 [] _ _ = []
neighbors3x3 m i j =
    [ edgeCase m row col
    | row <- [i - 1 .. i + 1]
    , col <- [j - 1 .. j + 1]
    ]

sumNeighbors3x3 :: Matrix -> Int -> Int -> Int
sumNeighbors3x3 m i j = sum (neighbors3x3 m i j)


checkRoll :: Matrix -> Int -> Int -> Int
checkRoll m i j = if sumNeighbors3x3 m i j > 4 then 1 else 0

allRolls :: Matrix -> Matrix
allRolls [] = []
allRolls m@(row0: _) = 
    [ [ checkRoll m row col
        | col <- [0 .. numCols -1]
        , (m !! row !! col) == 0
        ]
    | row <- [0 .. numRows - 1]
    ]
    where
        numRows = length m
        numCols = length row0

flatten :: Matrix -> [Int]
flatten m = concat m
