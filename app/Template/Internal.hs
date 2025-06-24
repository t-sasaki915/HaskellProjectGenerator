module Template.Internal ((</>)) where

import           Data.Text       (Text, unpack, pack)
import qualified System.FilePath as F

(</>) :: Text -> Text -> Text
x </> y = pack (unpack x F.</> unpack y)
