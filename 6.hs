ain :: IO ()
main = do
    content <- readFile "6.txt"
    let matrix = map words (lines content)
        mat = drop 1 (reverse matrix)
        operators = head (reverse matrix)

    print (compute (rows2cols (matCastToInt  mat)) operators)

rows2cols :: [[a]] -> [[a]]
rows2cols [] = []
rows2cols ([] : _) = []
rows2cols xss = map head xss : rows2cols (map (drop 1) xss)

matCastToInt :: [[String]] -> [[Int]]
matCastToInt = map (map read)

compute :: [[Int]] -> [String] -> Int
compute mat ops = 
    sum [ case op of
            "+" -> sum row
            "*" -> product row
        | (row, op) <- zip mat ops ]
