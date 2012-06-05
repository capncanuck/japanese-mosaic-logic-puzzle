module Main where

import Data.Functor ((<$>))
import System.Environment (getArgs)

import Parse (table, untable)
import Strategy (solve)

main :: IO ()
main = table <$> (head <$> getArgs >>= readFile) >>= putStrLn . untable . solve
