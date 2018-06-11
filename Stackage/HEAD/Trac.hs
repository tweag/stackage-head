{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.Trac
  ( generateTracTicket )
where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.History
import Text.URI (URI)
import qualified Text.URI as URI

-- | Generate Trac ticket.

generateTracTicket
  :: URI               -- ^ Link to this build
  -> (HistoryItem, HistoryItem) -- ^ History items that are being compared
  -> BuildDiff         -- ^ Build diff
  -> Text              -- ^ Rendered ticket
generateTracTicket thisBuildUrl hitems buildDiff =
  "Stackage HEAD has detected changes that may be of interest:\n\n" <>
  "{{{\n" <> prettyPrintBuildDiff hitems buildDiff <> "\n}}}\n\n" <>
  "To reproduce with Cabal, the following steps are recommended:\n" <>
  "  * Checkout commit " <> ghcCommit <> " of GHC, build, and install it locally;\n" <>
  "  * Edit `~/.cabal/config` and tell Cabal to use the GHC you've just compiled;\n" <>
  "  * Locate and clone repo of the offending package(s);\n" <>
  "  * Download this " <> cabalConfig <> " and put it in the cloned repo as `cabal.project.freeze`;\n" <>
  "  * Execute `cabal new-build` or `cabal new-test` and see if you can reproduce the failure.\n\n" <>
  "The full CircleCI build log is available online here: " <> URI.render thisBuildUrl
  where
    ghcCommit   = (URI.render . ghcCommitUrl   . snd) hitems
    cabalConfig = (URI.render . cabalConfigUrl . snd) hitems
