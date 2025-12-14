main :: IO ()
main = do
    contents <- readFile "1.txt"
    let content_parsed = pipeline (lines contents)
        instructions = map parseInstr content_parsed
        xs = scanl (+) 50 instructions
        hits = zipWith zeroHits xs (drop 1 xs)
    print (sum hits)

pipeline :: [String] -> [String]
pipeline = filterEmpty . map (unwords . words)

filterEmpty :: [String] -> [String]
filterEmpty = filter (not . null)

parseInstr :: String -> Int
parseInstr ('R':xs) = read xs
parseInstr ('L':xs) = negate (read xs)
parseInstr _        = 0

zeroHits :: Int -> Int -> Int
zeroHits x0 x1 =
    let a = min x0 x1
        b = max x0 x1
        hitsClosed = countMultiples100 a b
        startIsZero = if x0 `mod` 100 == 0 then 1 else 0
    in hitsClosed - startIsZero

countMultiples100 :: Int -> Int -> Int
countMultiples100 a b
    | a > b = 0
    | otherwise = floorDiv b 100 - ceilDiv a 100 + 1

floorDiv :: Int -> Int -> Int
floorDiv n d =
    let (q, r) = quotRem n d
    in if r/=0 && ((r > 0) /= (d > 0)) then q - 1 else q

ceilDiv :: Int -> Int -> Int
ceilDiv n d =
    let (q, r) = quotRem n d
    in if r/=0 && ((r > 0) == (d > 0)) then q + 1 else q
