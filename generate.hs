
queens = 4

atomify (x, y) = "q" ++ show x ++ show y

andify a b = "(and " ++ atomify a ++ " " ++ atomify b ++ ")"

getCons l = concat (map (\(a, b) -> "(declare-const q" ++ show a ++ show b ++ " Bool)\n") l)

minLine l = "(assert (or" ++ concat (map (\e -> " " ++ atomify e) l) ++ "))"

maxLine l ll n = "(assert (not (or" ++ go l ll n
    where
        go l [] n = "))))\n"
        go l (x : []) n = go (tail l) (tail l) (n - 1)
        go l (x : y : xs) n = " " ++ andify x y ++ go l (x : xs) n

maxLines l n = concat $  map (\e -> maxLine e e n) (take n $ taker n l)

minLines [] = []
minLines l@(x : xs) = minLine x ++ "\n" ++ minLines xs

taker n [] = []
taker n l = take n l : taker n (drop n l)

numbers = [(i,j) | i <- [0..queens - 1], j <- [0..queens - 1]]


main = do

    putStrLn $ getCons (numbers)
    putStrLn $ minLines $ taker queens numbers
    putStrLn $ maxLines numbers queens

