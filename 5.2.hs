import qualified Data.Set as Set
import qualified Data.List as List
import Data.Set (Set)

main :: IO ()
main = do
    content <- readFile "5.txt"
    let result@(ranges : ids) = splitParagraphs content

    print (length (compute (parseRanges ranges)))

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

compute :: [(Int, Int)] -> Set Int
compute ranges = 
    List.foldl' step Set.empty (List.sort ranges)
    where
        step :: Set Int -> (Int, Int) -> Set Int
        step acc (a,b) = acc `Set.union` Set.fromList [a..b]
