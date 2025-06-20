module Main (main) where

import           Control.Monad.Extra      (whenJust)
import           Control.Monad.IO.Class   (liftIO)
import           Data.String.Here         (i)
import           Data.Text                (Text, pack)
import qualified Data.Text.IO             as TextIO
import           System.Console.Haskeline

data Inputs = Inputs
    { input1 :: Text
    , input2 :: Text
    , input3 :: Text
    } deriving Show

askInputs :: InputT IO Inputs
askInputs =
    Inputs
        <$> askQuestion "Input1?" (Just "ABC")
        <*> askQuestion "Input2?" Nothing
        <*> askQuestion "Input3?" (Just "!#*(!U(J))")

main :: IO ()
main = do
    putStrLn "Haskell Project Generator"
    putStrLn ""

    inputs <- runInputT defaultSettings askInputs

    putStrLn (show inputs)

askQuestion :: Text -> (Maybe Text) -> InputT IO Text
askQuestion title maybeDefaultValue = do
    liftIO $ TextIO.putStrLn title

    whenJust maybeDefaultValue $ \defaultValue ->
        liftIO $ TextIO.putStrLn [i|Default: ${defaultValue}|]

    getInputLine "Question> " >>= \case
        Nothing -> askQuestion title maybeDefaultValue
        Just "" ->
            case maybeDefaultValue of
                Just defaultValue -> return defaultValue
                Nothing           -> askQuestion title maybeDefaultValue
        Just value -> return (pack value)
