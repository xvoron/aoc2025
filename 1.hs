newtype Circle = Circle Int
    deriving (Eq, Show)

mkCircle :: Int -> Circle
mkCircle x = Circle (x `mod` 100)

instance Num Circle where
    (Circle a) + (Circle b) = mkCircle (a + b)
    (Circle a) - (Circle b) = mkCircle (a - b)
    (Circle a) * (Circle b) = mkCircle (a * b)
    abs (Circle a)    = mkCircle (abs a)
    signum (Circle a) = Circle (signum a)
    fromInteger n     = mkCircle (fromInteger n)

main :: IO ()
main = do
    contents <- readFile "1.txt"
    let instructions = pipeline (lines contents)
        result = applyInstr (mkCircle 50) instructions
    print (countZeros result)

trim :: String -> String
trim = unwords . words


filterEmpty :: [String] -> [String]
filterEmpty = filter (not . null)

parseInstr :: String -> (Circle -> Circle)
parseInstr ('R':xs) = (\x -> x + mkCircle (read xs))
parseInstr ('L':xs) = (\x -> x - mkCircle (read xs))
parseInstr _        = id

applyInstr :: Circle -> [String] -> [Circle]
applyInstr start xs = scanl (\acc f -> f acc) start (map parseInstr xs)

countZeros :: [Circle] -> Int
countZeros xs = sum (map (\x -> if x == 0 then 1 else 0) xs)

pipeline :: [String] -> [String]
pipeline = filterEmpty . map trim
