module Main (main) where

import           Input

main :: IO ()
main = do
    putStrLn "Haskell Project Generator"
    putStrLn ""

    inputs <- askInputs

    print inputs
