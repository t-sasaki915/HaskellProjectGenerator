module Input (Inputs (..), askInputs) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Time.Calendar     (toGregorian)
import           Data.Time.Clock        (UTCTime (utctDay), getCurrentTime)
import           System.Console.Ask     (ask, askOrElse, defaultBehaviour,
                                         runAsk)

data Inputs = Inputs
    { projectName        :: Text
    , projectDirectory   :: Text
    , projectRepository  :: Text
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
    , needChangelog      :: Bool
    , needExecutable     :: Bool
    , needLibrary        :: Bool
    , needTestSuite      :: Bool
    , projectCreatedYear :: Integer
    } deriving Show

askInputs :: IO Inputs
askInputs = runAsk defaultBehaviour $ do
    now <- liftIO getCurrentTime
    let (currentYear, _, _) = toGregorian $ utctDay now

    projectName'        <- ask       "Project name?"
    projectDirectory'   <- askOrElse "Project directory?"   projectName'
    projectRepository'  <- askOrElse "Project repository?"  ("https://github.com/t-sasaki915/" <> projectName')
    projectVersion'     <- askOrElse "Project version?"     "0.1.0.0"
    projectHomepage'    <- askOrElse "Project homepage?"    (projectRepository' <> "#readme")
    projectBugReports'  <- askOrElse "Project bug-reports?" (projectRepository' <> "/issues")
    projectDescription' <- askOrElse "Project description?" ("Please see the README on GitHub at <" <> projectHomepage' <> ">")
    projectAuthor'      <- askOrElse "Project author?"      "Toma Sasaki"
    projectMaintainer'  <- askOrElse "Project maintainer?"  "netst915@gmail.com"
    projectLicence'     <- askOrElse "Project licence?"     "MIT"
    projectCopyright'   <- askOrElse "Project copyright?"   (Text.show currentYear <> " " <> projectAuthor')
    stackResolver'      <- askOrElse "Stack resolver?"      "nightly-2025-05-28"
    compilerVersion'    <- askOrElse "Compiler version?"    "ghc-9.12.2"
    needChangelog'      <- askOrElse "Need Changelog?"      False
    needExecutable'     <- askOrElse "Need executable?"     True
    needLibrary'        <- askOrElse "Need library?"        True
    needTestSuite'      <- askOrElse "Need test suite?"     True

    pure Inputs
        { projectName        = projectName'
        , projectDirectory   = projectDirectory'
        , projectRepository  = projectRepository'
        , projectVersion     = projectVersion'
        , projectHomepage    = projectHomepage'
        , projectBugReports  = projectBugReports'
        , projectDescription = projectDescription'
        , projectAuthor      = projectAuthor'
        , projectMaintainer  = projectMaintainer'
        , projectLicence     = projectLicence'
        , projectCopyright   = projectCopyright'
        , stackResolver      = stackResolver'
        , compilerVersion    = compilerVersion'
        , needChangelog      = needChangelog'
        , needExecutable     = needExecutable'
        , needLibrary        = needLibrary'
        , needTestSuite      = needTestSuite'
        , projectCreatedYear = currentYear
        }
