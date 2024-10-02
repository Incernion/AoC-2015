module C1 where

--       braces    accum  result
part1 :: String -> Int -> Int
part1 [] p = p
part1 (x:xs) acc
    | x == ')' = part1 xs (acc - 1)
    | x == '(' = part1 xs (acc + 1)
    | otherwise = error "idk"

--       braces    posacc    accum     result
part2 :: String -> Int -> Int    -> Int
part2 [] p _ = p
part2 _ posacc (-1) = posacc
part2 (x:xs) posacc acc
    | x == ')' = part2 xs (posacc + 1) (acc - 1)
    | x == '(' = part2 xs (posacc + 1) (acc + 1)
    | otherwise = error "idk"