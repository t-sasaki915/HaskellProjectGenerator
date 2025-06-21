module Main (main) where

import           Input (askInputs)

main :: IO ()
main = do
    putStrLn "Haskell Project Generator"

    inputs <- askInputs

    putStrLn (show inputs)
