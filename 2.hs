main :: IO ()
main = do
    content <- readFile "2.txt"
    let stringRanges = splitLine content
        ranges = map parseRange stringRanges
        realRanges = map (\(a, b) -> [a..b]) ranges
        evenRanges = map keepEvenLength realRanges
        finalNumbers = concatMap process evenRanges
    print (sum finalNumbers)


-- split line using , as delimiter
splitLine :: String -> [String]
splitLine line = words (map (\c -> if c == ',' then ' ' else c) line)

parseRange :: String -> (Int, Int)
parseRange s =
    let (a, '-' : b) = break (== '-') s
    in (read a, read b)

keepEvenLength :: [Int] -> [Int]
keepEvenLength xs = filter (\x -> even (length (show x))) xs

splitInHalf :: Int -> (Int, Int)
splitInHalf n =
    let half = length (show n) `div` 2
    in (read (take half (show n)), read (drop half (show n)))

process :: [Int] -> [Int]
process xs = do
    -- for each number in xs, split it in half and check if both halves are equal
    -- if they are equal, keep the number, otherwise discard it
    let halves = map splitInHalf xs
    [x | (x, (a, b)) <- zip xs halves, a == b]
