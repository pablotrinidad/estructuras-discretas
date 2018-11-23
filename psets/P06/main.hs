module Main where

import System.IO
import Game


main :: IO()
main = do
    -- Set global string
    let gameInstructions = unlines [
                            "\nInstrucciones:",
                            "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod",
                            "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,",
                            "quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo",
                            "consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse",
                            "cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non",
                            "proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n",
                            "Presiona intro para continuar..."
                        ]

    -- Print menu
    putStrLn "[1] Jugar 🎮"
    putStrLn "[2] Instrucciones 📜"
    putStrLn "[3] Salir ❎"

    input <- getLine
    case input of "1"     -> do startGame; main
                  "2"     -> do putStrLn gameInstructions; main
                  "3"     -> putStrLn "Ok ciao 👋"
                  _     -> do putStrLn "Elige una opción válida!\n"; main

    return ()

