{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Internal.HaskellDocument
-- Copyright   : (c) 2013 Montez Fitzpatrick
-- License     : MIT
-- Maintainer  : Montez Fitzpatrick <montezf@gmail.com>
-- Portability : non-portable
--
-- 'HaskellDocument' data structure for format-neutral representation of documents.  
-- This module format is heavily influenced by Pandoc.
module Internal.HaskellDocument where

import Control.Monad.ST
import Data.Generics (Data)
import Data.Typeable (Typeable)
import Data.String
import Data.Hashable (Hashable)
import qualified Data.Map as M
import Data.XML.Types
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.Extension (KnownExtension(..))

data HaskellDocument = HaskellDocument Meta Components
              deriving (Eq, Ord, Read, Show, Typeable, Data)

newtype Meta = Meta { getMeta :: M.Map String MetaValue }
               deriving (Eq, Ord, Read, Show, Typeable, Data)

data MetaValue = MetaMap (M.Map String MetaValue)
               | MetaPragma Pragma
               | MetaStr Text
               deriving (Eq, Ord, Read, Show, Typeable, Data)
               
-- Helper functions to extract metadata

-- | Initialize metadata
initMeta :: Meta
initMeta = Meta M.empty

-- | Retrieve the metadata value for a given @key@.
lookupMeta :: String -> Meta -> Maybe MetaValue
lookupMeta key (Meta m) = M.lookup key m

-- | Insert metadata value for a given @key@.
insertMeta :: String -> MetaValue -> Meta -> Meta
insertMeta k v (Meta m) = Meta $ M.insert k v m

-- | Components represent the fixed segmented portion of a Haskell Module Document
-- at least as far as the representation goes for our purposes.
data Components = Components ModuleDecl DataTypeDecl FuncDecl
    deriving (Eq, Ord, Read, Show, Typeable, Data)

data ModuleDecl 
    = ModuleDecl 
    { moduleDesc :: [Text]
    , moduleImports :: Set Text
    , moduleExports :: Set Text
    } deriving (Eq, Ord, Read, Show, Typeable, Data)

newtype DataTypeDecl = DataTypeDecl {getDataTD :: M.Map String [DataRecord]}
    deriving (Eq, Ord, Read, Show, Typeable, Data)

newtype FuncDecl = FuncDecl {getfuncDecl :: M.Map String [Text]}
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- NOTE: if we keep a set of datatypes that need to be created and a set of datatypes that
--   have been created, then the difference from the created set and need set will be the TL
--   data types and they will need to be exported.

-- NOTE: PCDATA or CDATA in an element is to be a lazy bytestring.  Else Strict Text
    
data DataRecord
    = DRConstructor (M.Map String [DataRecord])
    | DRField FieldInline FieldName FieldQualifer FieldType FieldComment
    | DRDeriving [Text]
    | DRTLDecl (M.Map String [DataRecord])  -- This is to be sent up the chain as an attribute that needs a top level declaration.  Usually used for options.
    deriving (Eq, Ord, Read, Show, Typeable, Data)
    -- still need an elegant way of handling mixed content
    
data FieldQualifer
    = FQNone       -- ^ One occurance of an (element) / Required Attribute
    | FQList       -- ^ Minimum of one occurance of an (element+) 
    | FQMaybe      -- ^ Zero or one occurances of an (element?) / Optional Attribute
    | FQMaybeList  -- ^ Zero or more occurances of an (element*)
    deriving (Eq, Ord, Read, Show, Typeable, Data)
    
data FieldInline
    = FNone      -- ^ Nothing
    | FInline    -- ^ Comma
    | FNoInline  -- ^ Pipe
    deriving (Eq, Ord, Read, Show, Typeable, Data)
    
data Pragma = Pragma
    { languageExtension :: [Text] -- temporary. See tODO below.
    , pragmaOptions     :: [Text]
    } deriving (Eq, Ord, Read, Show, Typeable, Data)
    
type FieldName = Text
type FieldType = Text -- could add some checking here in the future if needed
type FieldComment = Maybe [Text]
type ParamEntity = Text
type Comment = Text

-- utility

-- | knownExtension is depreciated, need that function here.  It generates a 
--   list of Language extensions.
languageExtensionList :: [Text]
languageExtensionList = map (T.pack . show) 
                              ([minBound .. maxBound] :: [KnownExtension])
                              
-- TODO: use a Set with ST monad
addLanguageExtension :: String -> [Text]
addLanguageExtension = undefined
