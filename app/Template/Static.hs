{-# LANGUAGE TemplateHaskell #-}

module Template.Static (staticTemplates) where

import           Data.ByteString   (ByteString)
import           Data.FileEmbed    (embedFile)
import           Data.Text         (Text)

import           Input             (Inputs (..))
import           Template.Internal ((</>))

staticTemplates :: Inputs -> [(Text, ByteString)]
staticTemplates inputs =
    [ (projDir </> ".gitignore"           , gitignore)
    , (projDir </> ".hlint.yaml"          , hlintYaml)
    , (projDir </> ".stylish-haskell.yaml", stylishHaskellYaml)
    , (projDir </> "LICENSE"              , license)
    , (projDir </> "Setup.hs"             , setupHs)
    ]
    <> [ (projDir </> "app"  </> "Main.hs", mainHs) | needExecutable inputs ]
    <> [ (projDir </> "src"  </> "Lib.hs" , libHs)  | needLibrary inputs ]
    <> [ (projDir </> "test" </> "Spec.hs", specHs) | needTestSuite inputs]

    where
        projDir = projectDirectory inputs

gitignore :: ByteString
gitignore = $(embedFile "static/.gitignore")

hlintYaml :: ByteString
hlintYaml = $(embedFile "static/.hlint.yaml")

stylishHaskellYaml :: ByteString
stylishHaskellYaml = $(embedFile "static/.stylish-haskell.yaml")

license :: ByteString
license = $(embedFile "static/LICENSE")

libHs :: ByteString
libHs = $(embedFile "static/Lib.hs")

mainHs :: ByteString
mainHs = $(embedFile "static/Main.hs")

setupHs :: ByteString
setupHs = $(embedFile "static/Setup.hs")

specHs :: ByteString
specHs = $(embedFile "static/Spec.hs")
