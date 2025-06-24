module Main (main) where

import           Control.Monad    (forM_)
import qualified Data.Text        as Text
import qualified Data.Text.IO     as TextIO
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath  (takeDirectory)

import           Input            (askInputs)
import           Template         (templates)

main :: IO ()
main = do
    putStrLn "Haskell Project Generator"
    putStrLn ""

    inputs <- askInputs

    forM_ (templates inputs) $ \(filePath', content) -> do
        TextIO.putStrLn ("Creating " <> filePath' <> " ...")

        let filePath = Text.unpack filePath'

        doesFileExist filePath >>= \case
            False -> do
                createDirectoryIfMissing True (takeDirectory filePath)

                TextIO.writeFile filePath content

                TextIO.putStrLn ("Created " <> filePath' <> " .")

            True ->
                TextIO.putStrLn ("Skipped " <> filePath' <> " because the file is existing.")

