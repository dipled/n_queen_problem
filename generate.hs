queens :: Int
queens = 5

atomify :: (Show a, Show b) => (a, b) -> String
atomify (x, y) = "q" ++ show x ++ show y

andify :: (Show a, Show b, Show c, Show d) => (a, b) -> (c, d) -> String
andify a b = "(and " ++ atomify a ++ " " ++ atomify b ++ ")"

getCons :: (Show a, Show b) => [(a, b)] -> String
getCons l = concat (map (\(a, b) -> "(declare-const q" ++ show a ++ show b ++ " Bool)\n") l)

minCell :: (Show a, Show b) => [(a, b)] -> String
minCell l = "(assert (or" ++ concat (map (\e -> " " ++ atomify e) l) ++ "))"

minCells :: (Show a, Show b) => [[(a, b)]] -> String
minCells [] = []
minCells l@(x : xs) = minCell x ++ "\n" ++ minCells xs

maxCell :: (Show a, Show b) => [(a, b)] -> [(a, b)] -> Int -> String
maxCell l ll n = "(assert (not (or" ++ go l ll n
  where
    go l [] n = ")))\n"
    go l (x : []) n = go (tail l) (tail l) (n - 1)
    go l (x : y : xs) n = " " ++ andify x y ++ go l (x : xs) n

maxCells :: (Show a, Show b) => [(a, b)] -> Int -> String
maxCells l n = concat $ map (\e -> maxCell e e n) (take n $ divider n l)

divider :: Int -> [a] -> [[a]]
divider n [] = []
divider n l = take n l : divider n (drop n l)

cellLines :: [(Int, Int)]
cellLines = [(i, j) | i <- [0 .. queens - 1], j <- [0 .. queens - 1]]

cellColumns :: [(Int, Int)]
cellColumns = [(j, i) | i <- [0 .. queens - 1], j <- [0 .. queens - 1]]

diagonal :: [[(Int, Int)]]
diagonal = [[(i, j) | i <- [0 .. queens - 1], j <- [0 .. queens - 1], i - j == k] | k <- [-queens + 2 .. queens - 2]]

antidiagonal :: [[(Int, Int)]]
antidiagonal = [[(i, j) | i <- [0 .. queens - 1], j <- [0 .. queens - 1], i + j == k] | k <- [1 .. 2 * queens - 3]]

main :: IO ()
main = do
   let s1 = ";constantes\n" ++ getCons cellLines
   let s2 = ";cada linha possui ao menos uma rainha\n" ++ minCells (divider queens cellLines)
   let s3 = ";cada linha possui no máximo uma rainha\n" ++ maxCells cellLines queens
   let s4 = ";cada coluna possui no máximo uma rainha\n" ++ maxCells cellColumns queens
   let s5 = ";cada diagonal possui no máximo uma rainha\n" ++ concat (map (\e -> maxCells e queens) diagonal)
   let s6 = concat $ map (\e -> maxCells e queens) antidiagonal
   writeFile (show queens ++ "queens.smt") $ s1 ++ "\n" ++ s2 ++ "\n" ++ s3 ++ "\n" ++ s4 ++ "\n" ++ s5 ++ s6 ++ "\n(check-sat)\n(get-model)"
