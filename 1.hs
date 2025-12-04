main :: IO ()
main = do
    contents <- readFile "test.txt"
    let instructions = pipeline (lines contents)
        result = applyInstr (0, 50) instructions
    let zeros = countZeros result
    let quotients = countQuotients result
    print(zeros)
    print(sum quotients)
    print(result)
    print(quotients)


filterEmpty :: [String] -> [String]
filterEmpty = filter (not . null)

parseInstr :: String -> ((Int, Int) -> (Int, Int))
parseInstr ('R':xs) = \(_, x) -> divMod (x + read xs) 100
parseInstr ('L':xs) = \(_, x) -> divMod (x - read xs) 100
parseInstr _        = \(_, x) -> divMod x 100

applyInstr :: (Int, Int) -> [String] -> [(Int, Int)]
applyInstr start xs = scanl (\acc f -> f acc) start (map parseInstr xs)

countZeros :: [(Int, Int)] -> Int
countZeros xs = sum (map (\(_, r) -> if r == 0 then 1 else 0) xs)

countQuotients :: [(Int, Int)] -> [Int]
countQuotients xs =
    [ step ((q0, r0), (q1, r1))
    | ((q0, r0), (q1, r1)) <- zip xs (drop 1 xs)
    ]

step :: ((Int, Int), (Int, Int)) -> Int
step ((_, r0), (q1, r1))
    | r0 == 0 && q1 == 0                = 0
    | r0 == 0 && q1 /= 0                = abs q1
    | r0 /= 0 && q1 == 0 && r1 == 0     = 1
    | r0 /= 0                           = abs q1
    | otherwise                         = 0

pipeline :: [String] -> [String]
pipeline = filterEmpty . map (unwords . words)
