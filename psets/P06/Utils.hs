module Utils where

import Data.Char


-- Print list elements
p :: (Show a) => [(a, a)] -> IO()
p [] = return ()
p (x:xs) = do
    print $ x
    p xs


removeArticles :: String -> String
removeArticles = foldr(\w s -> if (map toLower w) `elem` ["the"] then s else w ++ s) [] . words


-- Unify given string: Remove spaces, dashes and change every char to lowercase
unifyStr :: String -> String
unifyStr = foldr (\c s -> if c `elem` "_-. " then s else (toLower c):s) "" . removeArticles
