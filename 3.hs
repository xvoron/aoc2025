main :: IO ()
main = do
    content <- readFile "3.txt"
    let splitLines  = lines content
        numbers     = map toInts splitLines
    print (sum (map process numbers))


toInts :: [Char] -> [Int]
toInts = map (\c -> read [c])

maxNum :: [Int] -> Int
maxNum [] = error "Empty list has no largest number"
maxNum [x] = x
maxNum (x:xs) = max x (maxNum xs)

positionMaxNum :: [Int] -> Int
positionMaxNum xs =
    let m = maxNum xs
    in case [ i | (x, i) <- zip xs [0..], x == m ] of
         (i:_) -> i
         []    -> error "positionMaxNum: max not found"

process :: [Int] -> Int
process xs =
    let posmax = positionMaxNum xs
        rightFromMax = drop (posmax + 1) xs

        (first, second) =
            if null rightFromMax
                then 
                    (positionMaxNum (take posmax xs), posmax)
                else 
                    let relPos = positionMaxNum rightFromMax
                    in (posmax, posmax + 1 + relPos)

        a = xs !! first
        b = xs !! second
        combinedString = show a ++ show b
        combinedVal = read combinedString :: Int
    in combinedVal
