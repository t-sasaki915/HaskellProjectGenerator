{-# LANGUAGE TemplateHaskell #-}

module Template.Static (staticTemplates) where

import           Data.FileEmbed     (embedFile)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)

import           Input              (Inputs (..))
import           Template.Internal  ((</>))

staticTemplates :: Inputs -> [(Text, Text)]
staticTemplates inputs =
    [ (projDir </> ".gitignore"           , gitignore)
    , (projDir </> ".hlint.yaml"          , hlintYaml)
    , (projDir </> ".stylish-haskell.yaml", stylishHaskellYaml)
    , (projDir </> "Setup.hs"             , setupHs)
    ]
    <> [ (projDir </> "app"  </> "Main.hs", mainHs) | needExecutable inputs ]
    <> [ (projDir </> "src"  </> "Lib.hs" , libHs)  | needLibrary inputs ]
    <> [ (projDir </> "test" </> "Spec.hs", specHs) | needTestSuite inputs]
    
    where
        projDir = projectDirectory inputs

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
