module Main where

import System.Random

main :: IO ()
main = guessingGame

data Input = Guess Integer | Unparseable | Quit deriving Show

parseStr :: String -> Input
parseStr "q" = Quit
parseStr a = case (reads a :: [(Integer, String)]) of
    [(a,"")] -> Guess a
    _ -> Unparseable

guess :: IO Input
guess = do
    putStrLn "Guess a number from 1 to 10 [q to quit]"
    l <- getLine
    let i = parseStr l
    return i
-- readInput :: Read String -> IO Input = do i <- readLn

chooseNumber :: IO Integer
chooseNumber = randomRIO (1, 10)

guessingGame :: IO ()
guessingGame = do
    n <- chooseNumber
    i <- guess
    case i of
        Quit -> putStrLn "Bye!"
        Unparseable -> putStrLn "Invalid number, try again"
        Guess x | x == n -> putStrLn "Congratulations, you got it"
        _ -> do
            putStrLn "Incorrect, bad luck"
            guessingGame