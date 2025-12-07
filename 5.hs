import qualified Data.Set as Set

main :: IO ()
main = do
    content <- readFile "5.txt"
    let result@(ranges : ids) = splitParagraphs content

        ranges_ = parseRanges ranges
        ids_ = parseIds ids

    print (checkAll ids_ ranges_)

deduplicate :: [Int] -> Set.Set Int
deduplicate = Set.fromList

splitParagraphs :: String -> [String]
splitParagraphs = map unlines . go . lines
    where
        go [] = []
        go xs =
            let (p, rest) = break null xs
            in p : go (drop 1 rest)

parseIds :: [String] -> [Int]
parseIds [] = []
parseIds (x : _) = map read (lines x)

parseRange :: String -> (Int, Int)
parseRange s =
    let (a, '-' : b) = break (== '-') s
    in (read a, read b)

parseRanges :: String -> [(Int, Int)]
parseRanges = map parseRange . lines

checkId :: Int -> [(Int, Int)] -> Int
checkId id ranges =
    if any (\(a, b) -> a <= id && id <= b) ranges
    then 1
    else 0

checkAll :: [Int] -> [(Int, Int)] -> Int
checkAll ids ranges = sum (map (\x -> checkId x ranges) ids)
