{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildInfo
  ( BuildInfo (..) )
where

import Data.Aeson
import Data.Text (Text)

-- | GHC build info as to be found in GHC metadata file. Extend this record
-- as necessary.

newtype BuildInfo = BuildInfo
  { biSha1 :: Text
    -- ^ SHA1 hash of GHC commit at which the bindist was compiled
  }

instance FromJSON BuildInfo where
  parseJSON = withObject "GHC build metadata" $ \o ->
    BuildInfo <$> o .: "sha1"
