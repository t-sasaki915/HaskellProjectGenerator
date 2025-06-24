module Template (templates) where

import           Data.Text         (Text)
import           Text.Heredoc      (heredoc)

import           Input             (Inputs (..))
import           Template.Internal ((</>))
import           Template.Static   (staticTemplates)

templates :: Inputs -> [(Text, Text)]
templates inputs =
    [ (projDir </> projectName inputs <> ".cabal", projectCabal inputs)
    , (projDir </> "stack.yaml"                  , stackYaml inputs)
    ]
    <> staticTemplates inputs

    where
        projDir = projectDirectory inputs

projectCabal :: Inputs -> Text
projectCabal inputs =
    [heredoc|cabal-version: 2.2

name:           ${projectName inputs}
version:        ${projectVersion inputs}
description:    ${projectDescription inputs}
homepage:       ${projectHomepage inputs}
bug-reports:    ${projectBugReports inputs}
author:         ${projectAuthor inputs}
maintainer:     ${projectMaintainer inputs}
copyright:      ${projectCopyright inputs}
license:        ${projectLicence inputs}
license-file:   LICENSE
build-type:     Simple
extra-source-files:
    README.md

source-repository head
    type: git
    location: ${projectRepository inputs}

$if needExecutable inputs
    executable ${projectName inputs}-exe
      main-is: Main.hs
      other-modules:
          Paths_${projectName inputs}
      autogen-modules:
          Paths_${projectName inputs}
      hs-source-dirs:
          app
      ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints -threaded -rtsopts -with-rtsopts=-N
      build-depends:
        $if needLibrary inputs
            ${projectName inputs}
          , base >=4.7 && <5
        $else
            base >=4.7 && <5
      default-language: Haskell2010
      default-extensions: OverloadedStrings, LambdaCase, QuasiQuotes

$if needLibrary inputs
    library
      exposed-modules:
          Lib
      other-modules:
          Paths_${projectName inputs}
      autogen-modules:
          Paths_${projectName inputs}
      hs-source-dirs:
          src
      ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints
      build-depends:
          base >=4.7 && <5
      default-language: Haskell2010
      default-extensions: LambdaCase, OverloadedStrings, QuasiQuotes

$if needTestSuite inputs
    test-suite ${projectName inputs}-test
      type: exitcode-stdio-1.0
      main-is: Spec.hs
      other-modules:
          Paths_${projectName inputs}
      autogen-modules:
          Paths_${projectName inputs}
      hs-source-dirs:
          test
      ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints -threaded -rtsopts -with-rtsopts=-N
      build-depends:
        $if needLibrary inputs
            ${projectName inputs}
          , base >=4.7 && <5
        $else
            base >=4.7 && <5
      default-language: Haskell2010
      default-extensions: LambdaCase, OverloadedStrings, QuasiQuotes
    |]

stackYaml :: Inputs -> Text
stackYaml inputs =
    [heredoc|resolver: ${stackResolver inputs}
compiler: ${compilerVersion inputs}

packages:
- .
    |]
