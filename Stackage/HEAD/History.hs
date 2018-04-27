{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Stackage.HEAD.History
  ( History
  , HistoryItem (..)
  , strHistoryItem
  , historyItemToBuildUrl
  , hip
  , loadHistory
  , saveHistory
  , presentInHistory
  , extendHistory
  , twoLastHistoryItems
  , splitHistory
  , historyItems
  , cabalConfigUrl
  , ghcCommitUrl
  , decomposeHistoryItem )
where

import Control.Monad (void)
import Data.Bifunctor (second)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, fromJust)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void
import Path.IO (forgivingAbsence)
import System.FilePath (replaceExtension)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, hexDigitChar, string)
import Text.URI (URI)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Vector          as V
import qualified Text.URI             as URI
import qualified Text.URI.QQ          as Q

-- | Build history.

newtype History = History (Vector HistoryItem)

-- | Build history item.

newtype HistoryItem = HistoryItem
  { unHistoryItem :: Text
  } deriving (Eq, Hashable)

instance Csv.FromRecord HistoryItem where
  parseRecord v = do
    bs <- v Csv..! 0
    (return . HistoryItem . TE.decodeUtf8) bs

instance Csv.ToRecord HistoryItem where
  toRecord (HistoryItem txt) =
    Csv.record [TE.encodeUtf8 txt]

-- | Convert 'HistoryItem' to a 'String'.

strHistoryItem :: HistoryItem -> FilePath
strHistoryItem = T.unpack . unHistoryItem

-- | Get name of file where link to corresponding CircleCI build is stored.

historyItemToBuildUrl :: HistoryItem -> FilePath
historyItemToBuildUrl = flip replaceExtension "url" . T.unpack . unHistoryItem

-- | Get a more friendly form of history item for showing to users.

hip :: HistoryItem -> Text
hip (HistoryItem txt) = T.takeWhile (/= '.') txt

-- | Load history from given history file. If the file does not exist empty
-- 'History' is returned.

loadHistory
  :: FilePath          -- ^ Location of history file
  -> IO (Either String History)
loadHistory path = do
  mhistory <- forgivingAbsence (BL.readFile path)
  return $ case mhistory of
    Nothing    -> Right (History V.empty)
    Just bytes ->
      second History (Csv.decode Csv.NoHeader bytes)

-- | Save history to given history file.

saveHistory
  :: FilePath          -- ^ Location of history file
  -> History           -- ^ History to save
  -> IO ()
saveHistory path (History history) =
  BL.writeFile path (Csv.encode (V.toList history))

-- | Check if given item is present in build history.

presentInHistory :: History -> HistoryItem -> Bool
presentInHistory (History v) report = V.elem report v

-- | Append new item to the history.

extendHistory :: History -> HistoryItem -> History
extendHistory (History v) reportBasename = do
  History (V.cons reportBasename v)

-- | Get two last history items (in order: older, newer). If there are not
-- enough history itmes, return 'Nothing'.

twoLastHistoryItems :: History -> Maybe (HistoryItem, HistoryItem)
twoLastHistoryItems (History v)
  | V.length v < 2 = Nothing
  | otherwise      = Just (v V.! 1, v V.! 0)

-- | Split the history at given index. The first returned fragment of
-- 'History' will be of the specified length and the second one will contain
-- the rest.

splitHistory :: Int -> History -> (History, History)
splitHistory n (History v) = (History pre, History post)
  where
    (pre, post) = V.splitAt n v

-- | Get 'HistoryItem's as a list.

historyItems :: History -> [HistoryItem]
historyItems (History v) = V.toList v

-- | Produce URL at which cabal config corresponding to given Stackage build
-- plan can be found.

cabalConfigUrl :: URI.RText 'URI.PathPiece -> URI
cabalConfigUrl bp = [Q.uri|https://www.stackage.org|]
  { URI.uriPath = Just (False, bp :| [ [Q.pathPiece|cabal.config|] ])
  }

-- | Produce URL at which Phabricator will show the given commit of interest.

ghcCommitUrl :: URI.RText 'URI.PathPiece -> URI
ghcCommitUrl commit = [Q.uri|https://phabricator.haskell.org|]
  { URI.uriPath = Just (False, commit' :| [])
  }
  where
    -- NOTE This operation is guaranteed to be safe.
    commit' = fromJust $ URI.mkPathPiece ("rGHC" <> URI.unRText commit)

-- | Get target and GHC commit from a 'HistoryItem'.

decomposeHistoryItem
  :: HistoryItem
  -> (URI.RText 'URI.PathPiece, URI.RText 'URI.PathPiece)
decomposeHistoryItem (HistoryItem txt) =
  fromMaybe ([Q.pathPiece|UNKNOWN|], [Q.pathPiece|UNKNOWN|]) $ do
    (target, commit) <- parseMaybe pItem txt
    (,) <$> URI.mkPathPiece target
        <*> URI.mkPathPiece commit

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
