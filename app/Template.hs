module Template (templates) where

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Text.Encoding (encodeUtf8)
import           Text.Heredoc       (heredoc)

import           Input              (Inputs (..))
import           Template.Internal  ((</>))
import           Template.Static    (staticTemplates)

templates :: Inputs -> [(Text, ByteString)]
templates inputs =
    [ (projDir </> projectName inputs <> ".cabal", projectCabal inputs)
    , (projDir </> "stack.yaml"                  , stackYaml inputs)
    , (projDir </> "README.md"                   , readme inputs)
    , (projDir </> "LICENSE"                     , license inputs)
    ]
    <> [(projDir </> "CHANGELOG.md", changelog inputs) | needChangelog inputs]
    <> staticTemplates inputs

    where
        projDir = projectDirectory inputs

projectCabal :: Inputs -> ByteString
projectCabal inputs = encodeUtf8
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
    $if needChangelog inputs
        CHANGELOG.md

source-repository head
  type: git
  location: ${projectRepository inputs}

$if needLibrary inputs
    library
      exposed-modules:
          Lib
      other-modules:
          Paths_${Text.replace "-" "_" (projectName inputs)}
      autogen-modules:
          Paths_${Text.replace "-" "_" (projectName inputs)}
      hs-source-dirs:
          src
      ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints
      build-depends:
          base >=4.7 && <5
      default-language: Haskell2010
      default-extensions: LambdaCase, OverloadedStrings, QuasiQuotes

$if needExecutable inputs
    executable ${projectName inputs}-exe
      main-is: Main.hs
      other-modules:
          Paths_${Text.replace "-" "_" (projectName inputs)}
      autogen-modules:
          Paths_${Text.replace "-" "_" (projectName inputs)}
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

$if needTestSuite inputs
    test-suite ${projectName inputs}-test
      type: exitcode-stdio-1.0
      main-is: Spec.hs
      other-modules:
          Paths_${Text.replace "-" "_" (projectName inputs)}
      autogen-modules:
          Paths_${Text.replace "-" "_" (projectName inputs)}
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

stackYaml :: Inputs -> ByteString
stackYaml inputs = encodeUtf8
    [heredoc|resolver: ${stackResolver inputs}
compiler: ${compilerVersion inputs}

packages:
- .
|]

readme :: Inputs -> ByteString
readme inputs = encodeUtf8
    [heredoc|# ${projectName inputs}
${projectDescription inputs}
|]

changelog :: Inputs -> ByteString
changelog inputs = encodeUtf8
    [heredoc|# ${projectVersion inputs}
Initial release.
|]

license :: Inputs -> ByteString
license inputs = encodeUtf8
    [heredoc|MIT License

Copyright (c) ${Text.show (projectCreatedYear inputs)} ${projectAuthor inputs}

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|]
