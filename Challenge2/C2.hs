module C2 where
import qualified Data.Char as Char
import Data.List (elemIndex, delete)

data Direction = True | False

split :: Char -> String -> [String] -- Repurposed words function lmao
split delimiter s =  case dropWhile (==delimiter) s of
                      "" -> []
                      s' -> w : split delimiter s'' 
                            where (w, s'') = break (==delimiter) s'

tuplifier :: [String] -> (Int, Int, Int)
tuplifier ls = (nums!!0, nums!!1, nums!!2)
    where
        nums = map (read :: String -> Int) ls

calcArea :: (Int, Int, Int) -> Int
calcArea (l, w, h) = 2*l*w + 2*w*h + 2*h*l + minimum [l*w, w*h, h*l]


nums :: String -> [(Int, Int, Int)] -- This was a part of part1 in a where clause but I need it later, soooo :p
nums content = map tuplifier stringNums
    where
        stringNums = map (split 'x') $ lines content

part1 :: String -> Int
part1 content = sum $ map calcArea (nums content)

--------------------------------

getShortest :: (Int, Int, Int) -> (Int, Int)
getShortest (x, y, z) = (shortest!!0, shortest!!1)
    where
        shortest = delete (maximum [x, y, z]) [x, y, z]

calculateRibbon :: (Int, Int, Int) -> Int
calculateRibbon dimensions@(x, y, z) = s1*2 + s2*2 + x*y*z
    where 
        (s1, s2) = getShortest dimensions

part2 :: String -> Int
part2 content = sum $ map calculateRibbon (nums content)