{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Readers.XSD
-- Copyright   : (c) 2013 Montez Fitzpatrick
-- License     : MIT
-- Maintainer  : Montez Fitzpatrick <montezf@gmail.com>
-- Portability : non-portable
--
module Readers.XSD where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.XML.Types (ExternalID(PublicID, SystemID),
                       Instruction(Instruction))
import Control.Monad.ST
import Data.STRef
import Control.Monad

import Internal.HaskellDocument

-- | Takes a XSD document and transforms it into a Haskell
--   Document.  N:M relationship.
parseXSD2HSDoc :: TL.Text -> HSComponents
parseXSD2HSDoc txt = runST $ do
  hsc <- newSTRef initHSC
  forM_ getXSD $ \x -> do
    a <- readSTRef hsc
    writeSTRef hsc (eval a x)

  readSTRef hsc
  where
    getXSD = dtdComponents $ parseXSD txt
    parseXSD = undefined

eval = undefined
