{-# LANGUAGE TemplateHaskell #-}

module Template.Static (staticTemplates) where

import           Data.FileEmbed     (embedFile)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           System.FilePath    ((</>))

import           Input              (Inputs (..))

staticTemplates :: Inputs -> [(FilePath, Text)]
staticTemplates inputs =
    [ (".gitignore"           , gitignore)
    , (".hlint.yaml"          , hlintYaml)
    , (".stylish-haskell.yaml", stylishHaskellYaml)
    , ("Setup.hs"             , setupHs)
    ]
    <> [ ("app"  </> "Main.hs", mainHs) | needExecutable inputs ]
    <> [ ("src"  </> "Lib.hs" , libHs)  | needLibrary inputs ]
    <> [ ("test" </> "Spec.hs", specHs) | needTestSuite inputs]

gitignore :: Text
gitignore = decodeUtf8 $(embedFile "static/.gitignore")

hlintYaml :: Text
hlintYaml = decodeUtf8 $(embedFile "static/.hlint.yaml")

stylishHaskellYaml :: Text
stylishHaskellYaml = decodeUtf8 $(embedFile "static/.stylish-haskell.yaml")

libHs :: Text
libHs = decodeUtf8 $(embedFile "static/Lib.hs")

mainHs :: Text
mainHs = decodeUtf8 $(embedFile "static/Main.hs")

setupHs :: Text
setupHs = decodeUtf8 $(embedFile "static/Setup.hs")

specHs :: Text
specHs = decodeUtf8 $(embedFile "static/Spec.hs")
