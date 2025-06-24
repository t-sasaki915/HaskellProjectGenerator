module Main (main) where

import           Control.Monad    (forM_)
import qualified Data.Text        as Text
import qualified Data.Text.IO     as TextIO
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath  (takeDirectory)
import           System.IO        (hFlush, stdout)

import           Input            (Inputs (projectName), askInputs)
import           Template         (templates)

main :: IO ()
main = do
    TextIO.putStrLn "Haskell Project Generator"
    TextIO.putStrLn ""

    inputs <- askInputs

    forM_ (templates inputs) $ \(filePath', content) -> do
        TextIO.putStr ("Creating " <> filePath' <> " ...")
        hFlush stdout

        let filePath = Text.unpack filePath'

        doesFileExist filePath >>= \case
            False -> do
                createDirectoryIfMissing True (takeDirectory filePath)

                TextIO.writeFile filePath content

                TextIO.putStrLn "OK."

            True ->
                TextIO.putStrLn ("Skipped because the file is existing.")

    TextIO.putStrLn ""
    TextIO.putStrLn ("Project " <> projectName inputs <> " has been created.")
