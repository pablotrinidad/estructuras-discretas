module Utils where

import Data.Char


-- Print list elements
p :: (Show a) => [(a, a)] -> IO()
p [] = return ()
p (x:xs) = do
    print $ x
    p xs


-- Concat strings: Given a list of strings, return a unique string


-- Remove articles like "the" from titles
removeArticles :: String -> String
removeArticles = foldr (\w s -> if (map toLower w) `elem` ["the"] then s else w ++ s) [] . words


-- Unify given string: Remove spaces, dashes and change every char to lowercase
unifyStr :: String -> String
unifyStr = foldr (\c s -> if c `elem` "_-. " then s else (toLower c):s) "" . removeArticles
