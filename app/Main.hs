module Main (main) where

import qualified Data.Text.IO as TextIO
import           Input
import           Template

main :: IO ()
main = do
    putStrLn "Haskell Project Generator"
    putStrLn ""

    inputs <- askInputs

    TextIO.putStrLn (projectCabal inputs)
    TextIO.putStrLn (stackYaml inputs)
