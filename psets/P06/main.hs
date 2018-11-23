module Main where

import Data.Char
import System.IO


p :: (Show a) => [(a, a)] -> IO()
p [] = return ()
p (x:xs) = do
    print $ x
    p xs

-- unifyName :: (String, String) -> String
-- unifyName = unlines . map (dropWhile isSpace) . lines

-- Unify movie name: Remove spaces and change every char to lowercase
unifyName :: String -> String
unifyName [] = []
unifyName (x:xs)
    | isSpace x = unifyName xs
    | otherwise = (toLower x) : unifyName xs


main :: IO()
main = do
    handle <- readFile "data/movies.txt"
    let movies = [(unifyName a, b) | (a, b) <- map (read :: String -> (String, String)) $ lines handle]

    p movies

    return ()

    -- print "[1] Jugar 🎮"
    -- print "[2] Instrucciones 📜"
    -- print "[3] Salir ❌"

