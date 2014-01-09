{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Readers.DTD
-- Copyright   : (c) 2013 Montez Fitzpatrick
-- License     : MIT
-- Maintainer  : Montez Fitzpatrick <montezf@gmail.com>
-- Portability : non-portable
--
module Readers.DTD where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.XML.DTD.Parse as LibDTD (parseDTD)
import Data.XML.DTD.Types
import Data.XML.Types (ExternalID(PublicID, SystemID),
                       Instruction(Instruction))
import Control.Monad.ST
import Data.STRef
import Control.Monad

import Internal.HaskellDocument

-- | Takes a DTD document and transforms it into a Haskell
--   Document.  N:M relationship.
parseDTD2HSDoc :: TL.Text -> HSComponents
parseDTD2HSDoc txt = runST $ do
  hsc <- newSTRef initHSC
  forM_ getDTD $ \x -> do
    a <- readSTRef hsc
    writeSTRef hsc (eval a x)

  readSTRef hsc
  where
    getDTD = dtdComponents $ LibDTD.parseDTD txt


eval :: HSComponents -> DTDComponent -> HSComponents
eval hsc@(HSComponents m d f) (DTDEntityDecl e) = evalEntityDecl e
eval hsc@(HSComponents m d f) (DTDElementDecl e) = evalElementDecl e
eval hsc@(HSComponents m d f) (DTDAttList a) = evalAttList a
eval hsc@(HSComponents m d f) (DTDNotation a) = evalNotation a
eval hsc@(HSComponents m d f) (DTDPERef p) = evalPERef p
eval hsc@(HSComponents m d f) (DTDInstruction a) = evalInstruction a
eval hsc@(HSComponents m d f) (DTDComment s) = evalComment s

evalEntityDecl = undefined
evalElementDecl = undefined
evalAttList = undefined
evalNotation = undefined
evalPERef = undefined
evalInstruction = undefined
evalComment = undefined
