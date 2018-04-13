{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.Trac
  ( generateTracTicket )
where

import Control.Monad (void)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.History
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, hexDigitChar, string)
import qualified Data.Text as T

-- | Generate Trac ticket.

generateTracTicket
  :: Maybe String      -- ^ Link to this build
  -> (HistoryItem, HistoryItem) -- ^ History items that are being compared
  -> BuildDiff         -- ^ Build diff
  -> Text              -- ^ Rendered ticket
generateTracTicket thisBuildUrl hitems buildDiff =
  "Stackage HEAD has detected changes that may be of interest:\n\n" <>
  "{{{\n" <> prettyPrintBuildDiff hitems buildDiff <> "\n}}}\n\n" <>
  "To reproduce with Cabal, the following steps are recommended:\n" <>
  "  * Checkout commit " <> commit <> " of GHC, build, and install it locally;\n" <>
  "  * Edit `~/.cabal/config` and tell Cabal to use the GHC you've just compiled;\n" <>
  "  * Locate and clone repo of the offending package(s);\n" <>
  "  * Download this " <> cabalConfig <> " and put it in the cloned repo as `cabal.project.freeze`;\n" <>
  "  * Execute `cabal new-build` or `cabal new-test` and see if you can reproduce the failure.\n\n" <>
  buildUrlNote

  where
    cabalConfig = "https://www.stackage.org/" <> target <> "/cabal.config"
    (target, commit) = decomposeHistoryItem (snd hitems)
    buildUrlNote =
      case thisBuildUrl of
        Nothing -> ""
        Just buildUrl ->
          "The full CircleCI build log is available online here: " <>
               T.pack buildUrl <> ".\n"

-- | Get target and GHC commit from a 'HistoryItem'.

decomposeHistoryItem :: HistoryItem -> (Text, Text)
decomposeHistoryItem (HistoryItem txt) =
  case parseMaybe pItem txt of
    Nothing -> ("<UNKNOWN>", "<UNKNOWN>")
    Just  x -> x

----------------------------------------------------------------------------
-- History item parser

type Parser = Parsec Void Text

pItem :: Parser (Text, Text)
pItem = do
  target <- pNightly
  void (string "-")
  commit <- pSha1
  void (string ".csv")
  return (target, commit)

pNightly :: Parser Text
pNightly = fmap fst . match $ do
  void (string "nightly-")
  void (count 4 digitChar)
  void (string "-")
  void (count 2 digitChar)
  void (string "-")
  void (count 2 digitChar)

pSha1 :: Parser Text
pSha1 = T.pack <$> count 40 hexDigitChar
