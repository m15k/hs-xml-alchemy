{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Internal.HaskellDocument
-- Copyright   : (c) 2013 Montez Fitzpatrick
-- License     : MIT
-- Maintainer  :  Montez Fitzpatrick <montezf@gmail.com>
-- Portability :  non-portable
--
-- 'HaskellDocument' data structure for format-neutral representation of documents.  
-- This module format is heavily influenced by Pandoc.
module Internal.HaskellDocument where

import Control.Monad.ST
import Data.Generics (Data, Typeable)
import Data.String
import Data.Hashable (Hashable)
import qualified Data.Map as M
import Data.XML.Types
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.HashTable.ST.Cuckoo
import Data.HashTable.Class (HashTable)
import Data.HashTable.IO (IOHashTable, CuckooHashTable)
import qualified Data.HashTable.IO as H
import qualified Language.Haskell.Extension (KnownExtension(..))

data HaskellDocument = HaskellDocument Meta Components
              deriving (Eq, Ord, Read, Show, Typeable, Data)

-- newtype Meta = Meta { getMeta :: M.Map String MetaValue }
-- newtype Meta = Meta { getMeta :: ST s (HashTable s String MetaValue }
newtype Meta = Meta { getMeta :: IO (HashTable String MetaValue }
               deriving (Eq, Ord, Read, Show, Typeable, Data)
               
type HashTable k v = CuckooHashTable k v
               
data MetaValue = MetaMap (M.Map String MetaValue)
               | MetaPragma Pragma
               | MetaStr Text
               deriving (Eq, Ord, Read, Show, Typeable, Data)
               
-- Helper functions to extract metadata

-- | Initialize metadata
initMeta :: Meta
initMeta = H.new

-- | Retrieve the metadata value for a given @key@.
-- lookupMeta :: String -> Meta -> Maybe MetaValue
-- lookupMeta key (Meta m) = M.lookup key m
lookupMeta :: String -> Meta -> Maybe MetaValue
lookupMeta key (Meta m) = m >>= \ht -> H.lookup ht key

-- | Insert metadata value for a given @key@.
insertMeta :: (Eq k, Hashable k, HashTable h) => IO (IOHashTable h k v) -> k -> v -> IO (IOHashTable h k v)
insertMeta m k v = m >>= \ht -> H.insert ht k v >> return ht

-- Components
data Components
    = ModuleDecl 
        { moduleDesc :: [Text], moduleImports :: Set Text, moduleExports :: Set Text}
    | DataTypeDecl (M.Map String [DataRecord])
    | FuncDecl (M.Map String [Text])
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
    
-- TODO: import Language Pragmas
-- data PragmaLanguage

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
                              ([minBound .. maxBound] :: [LHE.KnownExtension])
                              
-- TODO: use a Set with ST monad
addLanguageExtension :: String -> [Text]
addLanguageExtension = undefined