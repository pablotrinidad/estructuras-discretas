module Utils where

import Data.Char

-- Remove articles like "the" from titles
removeArticles :: String -> String
removeArticles = foldr (\w s -> if (map toLower w) `elem` ["the", "an", "a"] then s else w ++ s) [] . words


-- Unify given string: Remove spaces, dashes and change every char to lowercase
unifyStr :: String -> String
unifyStr = foldr (\c s -> if c `elem` "_-. " then s else (toLower c):s) "" . removeArticles
