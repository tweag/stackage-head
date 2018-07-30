{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stackage.HEAD.Site.Location
  ( overviewL
  , overviewUrl
  , buildL
  , diffL
  , packageL )
where

import Control.Monad.Catch
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Stackage.HEAD.History
import Stackage.HEAD.Package
import Stackage.HEAD.Site.Type
import qualified Text.URI  as URI

overviewL :: Location
overviewL = $(location ["index.html"])

overviewUrl :: Text
overviewUrl = URI.render (locationUri overviewL)

buildL :: MonadThrow m => HistoryItem -> m Location
buildL hi = mkLocation ("build" :| [hitemPretty hi <> ".html"])

diffL :: MonadThrow m => HistoryItem -> HistoryItem -> m Location
diffL hi0 hi1 = mkLocation ("diff" :| [x])
  where
    x = hitemPretty hi0 <> "-vs-" <> hitemPretty hi1 <> ".html"

packageL :: MonadThrow m => HistoryItem -> PackageName -> m Location
packageL hi (PackageName n) = mkLocation
  ("build" :| [hitemPretty hi, n, "overview.html"])
