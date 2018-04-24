{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stackage.HEAD.Package
  ( PackageName (..)
  , strPackageName )
where

import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
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
