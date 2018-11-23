module Game where

import Utils
import System.Random



game :: (Integral a) => a -> a -> IO()
game 0 _ = do
    print "GAME OVER 😭😭😭"
game _ _ = do
    print "ME LAPELAS"



startGame :: IO()
startGame = do

    -- Load movies data
    handle <- readFile "data/movies.txt"
    let movies = [(unifyStr a, b) | (a, b) <- map (read :: String -> (String, String)) $ lines handle]

    -- Choose random movie
    gen <- newStdGen
    let index = fst $ randomR (0, (length movies) - 1) gen
    let movie = movies !! index

    print movie

    return ()
