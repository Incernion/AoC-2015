module Main where

import qualified C1
import System.Environment (getArgs)

parseArgs :: [String] -> (Int, Int)
parseArgs lst = (ints!!0, ints!!1)
    where ints = map read lst :: [Int]

startModule :: (Int, Int) -> String -> String
startModule tuple content
    | tuple == (1, 1) = show $ C1.part1 content 0
    | tuple == (1, 2) = show $ C1.part2 content 0 0
    | otherwise = "Invalid Option"

main :: IO ()
main = do
    args <- getArgs
    let option = parseArgs args
    filecontent <- readFile ("Challenge" ++ show (fst option) ++ "/in")
    print $ startModule option filecontent