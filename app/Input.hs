module Input (Inputs (..), askInputs) where

import           Control.Monad.Extra      (whenJust)
import           Control.Monad.IO.Class   (liftIO)
import           Data.String.Here         (i)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as TextIO
import           Data.Typeable            (Typeable)
import           System.Console.Haskeline (InputT, defaultSettings,
                                           getInputLine, runInputT)
import           Text.Regex.Posix         ((=~))

data Inputs = Inputs
    { gitHubUrl          :: Text
    , projectName        :: Text
    , projectVersion     :: Text
    , projectHomepage    :: Text
    , projectBugReports  :: Text
    , projectDescription :: Text
    , projectAuthor      :: Text
    , projectMaintainer  :: Text
    , projectLicence     :: Text
    , projectCopyright   :: Text
    , stackResolver      :: Text
    , compilerVersion    :: Text
    , needExecutable     :: Bool
    , needLibrary        :: Bool
    , needTestSuite      :: Bool
    } deriving Show

askInputs :: IO Inputs
askInputs = runInputT defaultSettings $
    Inputs
        <$> askInput   "GitHub URL?"          Nothing
        <*> askInput   "Project name?"        Nothing
        <*> askInput   "Project version?"     (Just "1.0.0.0")
        <*> askInput   "Project homepage"     (Just "${gitHubUrl}#readme")
        <*> askInput   "Project bug-reports?" (Just "${gitHubUrl}/issues")
        <*> askInput   "Project description?" (Just "Please see the README on GitHub at <${projectHomepage}>")
        <*> askInput   "Project author?"      (Just "Toma Sasaki")
        <*> askInput   "Project maintainer?"  (Just "netst915@gmail.com")
        <*> askInput   "Project licence?"     (Just "MIT")
        <*> askInput   "Project copyright?"   (Just "${year} ${projectAuthor}")
        <*> askInput   "Stack resolver?"      (Just "nightly-2025-05-28")
        <*> askInput   "Compiler version?"    (Just "ghc-9.12.2")
        <*> askYesOrNo "Need executable?"     (Just True)
        <*> askYesOrNo "Need library?"        (Just True)
        <*> askYesOrNo "Need test suite?"     (Just True)

askInput :: Text -> (Maybe Text) -> InputT IO Text
askInput = ask "Input> " Just

askYesOrNo :: Text -> (Maybe Bool) -> InputT IO Bool
askYesOrNo = ask "Y/N> " $ \input ->
    let lower = Text.unpack (Text.toLower input) in
        if lower =~ ("^(t(rue)?|y(es)?|aye)$" :: String)
            then Just True
            else
                if lower =~ ("^(f(alse)?|no?)$" :: String)
                    then Just False
                    else Nothing

ask :: (Typeable a, Show a) => Text -> (Text -> Maybe a) -> Text -> (Maybe a) -> InputT IO a
ask prompt f title maybeDefaultValue = do
    liftIO $ do
        TextIO.putStrLn ""
        TextIO.putStrLn title

    whenJust maybeDefaultValue $ \defaultValue ->
        liftIO $ TextIO.putStrLn [i|Default: ${defaultValue}|]

    getInputLine (Text.unpack prompt) >>= \case
        Nothing -> ask prompt f title maybeDefaultValue
        Just "" ->
            case maybeDefaultValue of
                Just defaultValue -> return defaultValue
                Nothing           -> ask prompt f title maybeDefaultValue
        Just value ->
            case f (Text.pack value) of
                Just value' -> return value'
                Nothing     -> ask prompt f title maybeDefaultValue
