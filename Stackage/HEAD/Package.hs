{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stackage.HEAD.Package
  ( PackageName (..)
  , strPackageName
  , packageNameRelDir )
where

import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Data.String (IsString)
import Data.Text (Text)
import Path
import qualified Data.Csv  as Csv
import qualified Data.Text as T

-- | Package name.

newtype PackageName = PackageName
  { unPackageName :: Text
  } deriving ( Show
             , Eq
             , Ord
             , Hashable
             , Csv.ToField
             , Csv.FromField
             , IsString )

-- | Convert 'PackageName' to a 'String'.

strPackageName :: PackageName -> String
strPackageName = T.unpack . unPackageName

-- | Convert 'PackageName' to a @'Path' 'Rel' 'Dir'@. This is in assumption
-- that all package names are valid relative directory names.

packageNameRelDir :: PackageName -> Path Rel Dir
packageNameRelDir = fromJust . parseRelDir . strPackageName
