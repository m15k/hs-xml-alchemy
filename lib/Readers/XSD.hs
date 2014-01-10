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

eval :: HSComponents -> DTDComponent -> HSComponents
eval hsc@(HSComponents m d f) (DTDEntityDecl e) = evalEntityDecl e
eval hsc@(HSComponents m d f) (DTDElementDecl e) = evalElementDecl e
eval hsc@(HSComponents m d f) (DTDAttList a) = evalAttList a
eval hsc@(HSComponents m d f) (DTDNotation a) = evalNotation a
eval hsc@(HSComponents m d f) (DTDPERef p) = evalPERef p
eval hsc@(HSComponents m d f) (DTDInstruction a) = evalInstruction a
eval hsc@(HSComponents m d f) (DTDComment s) = evalComment s

evalPERef = undefined
evalComment = undefined

evalEntityDecl (InternalGeneralEntityDecl name [(EntityText s)]) = undefined
evalEntityDecl (InternalGeneralEntityDecl name [(EntityPERef s)]) = undefined
evalEntityDecl (ExternalGeneralEntityDecl name eid s) = undefined
evalEntityDecl (InternalParameterEntityDecl name [(EntityText s)]) = undefined
evalEntityDecl (InternalParameterEntityDecl name [(EntityPERef s)]) = undefined
evalEntityDecl (ExternalParameterEntityDecl name eid) = undefined

evalElementDecl (ElementDecl name ContentEmpty) = undefined
evalElementDecl (ElementDecl name ContentAny) = undefined
evalElementDecl (ElementDecl name (ContentElement c)) = undefined
evalElementDecl (ElementDecl name (ContentMixed xs)) = undefined

evalContentModel (CMName s r) = undefined
evalContentModel (CMChoice cm r) = undefined
evalContentModel (CMSeq cm r) = undefined

evalRepeat One = undefined
evalRepeat ZeroOrOne = undefined
evalRepeat ZeroOrMore = undefined
evalRepeat OneOrMore = undefined

evalAttList AttList{..} = undefined

evalAttDecl AttDecl{..} = undefined

evalAttType AttStringType = undefined
evalAttType AttIDType = undefined
evalAttType AttIDRefType = undefined
evalAttType AttIDRefsType = undefined
evalAttType AttEntityType = undefined
evalAttType AttEntitiesType = undefined
evalAttType AttNmTokenType = undefined
evalAttType AttNmTokensType = undefined
evalAttType (AttEnumType s) = undefined
evalAttType (AttNotationType s) = undefined

evalAttDefault AttRequired = undefined
evalAttDefault AttImplied = undefined
evalAttDefault (AttFixed s) = undefined
evalAttDefault (AttDefaultValue s) = undefined

evalNotation (Notation name (NotationSysID s)) = undefined
evalNotation (Notation name (NotationPubID s)) = undefined
evalNotation (Notation name (NotationPubSysID s0 s1)) = undefined 

evalInstruction Instruction{..} = undefined
