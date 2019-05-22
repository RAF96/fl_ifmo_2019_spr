module Main where

import System.Environment
import Expression
import Text.Printf

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        putStrLn $ input
        let a = parseExpression (init $ input)
        let r = executeExpression (init $ input)
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either show show a
        putStrLn $ either show show r
        putStrLn ""
    )
    fileNames
