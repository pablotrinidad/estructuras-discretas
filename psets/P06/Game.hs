module Game where

import Utils
import System.Random



game :: Int -> [(String, String)] -> IO()
game 0 _ = do
    putStrLn "GAME OVER 😭😭😭\n"
game lives movies = do
    -- Get random movie
    gen <- newStdGen
    let index = fst $ randomR (0, (length movies) - 1) gen
    let movie = movies !! index

    -- Show hint
    putStrLn $ "¿Cuál es el nombre de la película? " ++ (unwords . take lives $ repeat "♥️")
    putStrLn $ snd movie

    -- Uncomment when debugging!
    -- putStrLn $ fst movie

    -- Retrieve user input and compare it with actual movie title
    usrGuess <- getLine
    if (unifyStr usrGuess) == (unifyStr $ fst movie)
        then
            putStrLn "✅\n"
        else do
            putStrLn "❌"
            game (pred lives) movies  -- Run game again with one live less

    return ()


startGame :: IO()
startGame = do

    -- Load movies data
    handle <- readFile "data/movies.txt"
    let movies = map (read :: String -> (String, String)) $ lines handle

    -- Start a new game with 5 lives
    game 5 movies

    return ()
