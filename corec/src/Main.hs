module Main(main) where

import Corec.COR
import System.Environment

main = do
    putStrLn "How many Fibonacci numbers should I take?"
    n <- getLine
    putStrLn $ show $ take (read n :: Int) theFibs