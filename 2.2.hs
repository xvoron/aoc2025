import qualified Data.Set as Set

main :: IO ()
main = do
    content <- readFile "2.txt"
    let stringRanges = splitLine content
        ranges = map parseRange stringRanges
        realRanges = concat (map (\(a, b) -> [a..b]) ranges)
        strRanges = map show realRanges
        onlyRepeated = filter isRepeated strRanges
    print (sum (map read onlyRepeated :: [Int]))

-- split line using , as delimiter
splitLine :: String -> [String]
splitLine line = words (map (\c -> if c == ',' then ' ' else c) line)

parseRange :: String -> (Int, Int)
parseRange s =
    let (a, '-' : b) = break (== '-') s
    in (read a, read b)

isRepeated :: String -> Bool
isRepeated s =
    let n = length s
    in any (\k -> n `mod` k == 0
                && concat (replicate (n `div` k) (take k s)) == s)
        [1..n `div` 2]
