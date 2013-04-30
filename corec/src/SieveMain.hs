module Main(main) where

import Corec.COR
import System.Environment

main = do
    putStrLn "Enter list of integers for Arosthenes sieve"
    n <- getLine
    putStrLn $ show $ sieve' (read n)