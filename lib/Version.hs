-- |
-- Module      : Version.hs
-- Copyright   : (c) 2013 Montez Fitzpatrick
-- License     : MIT
-- Maintainer  : Montez Fitzpatrick <montezf@gmail.com>
-- Portability : non-portable
module Version (version) where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Version (showVersion)
import qualified Paths_xml_alchemy as Paths (version)

version :: Text
version = T.pack $ showVersion Paths.version