{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Stackage.HEAD.History
  ( -- * History Item
    HistoryItem
  , HistoryItemException
  , mkHistoryItem
  , hitemPretty
  , hitemPrettyStr
  , reportPath
  , logsPath
  , hitemTarget
  , hitemCommit
  , hitemBuildUrl
  , cabalConfigUrl
  , ghcCommitUrl
  -- * Build history
  , History
  , loadHistory
  , saveHistory
  , extendHistory
  , newestHistoryItem
  , twoNewestHistoryItems
  , splitHistory
  , historyItems )
where

import Control.Exception (Exception (..))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow (..))
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import GHC.Generics
import Path
import Path.IO (forgivingAbsence)
import Text.URI (URI)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified Text.URI             as URI
import qualified Text.URI.QQ          as Q

----------------------------------------------------------------------------
-- History item

-- | Build history item. A 'HistoryItem' is a combination of information
-- that identifies a build, that includes:
--
--     * Target name, which is build plan that we used, for example
--       @nightly-2018-04-04@.
--     * GHC commit SHA1 that tells which version of GHC has been tested.
--     * Build URL, which is a link to CircleCI build log (documenting how
--       we were building a /target/ build plan using particular version of
--       GHC) available online.

data HistoryItem = HistoryItem
  { historyItemTarget :: !Text -- ^ Target name
  , historyItemSha1 :: !Text -- ^ GHC commit SHA1
  , historyItemBuildUrl :: !URI -- ^ Link to the build
  } deriving (Generic)

instance Eq HistoryItem where
  HistoryItem t0 c0 _ == HistoryItem t1 c1 _ = t0 == t1 && c0 == c1

instance Ord HistoryItem where
  HistoryItem t0 c0 _ `compare` HistoryItem t1 c1 _ =
    case t0 `compare` t1 of
      EQ -> c0 `compare` c1
      r  -> r

instance Csv.FromRecord HistoryItem where
  parseRecord v = do
    let fromMonadThrow = either (fail . displayException) return
    target   <- v Csv..! 0
    commit   <- v Csv..! 1
    buildUrl <- (v Csv..! 2) >>= fromMonadThrow . URI.mkURI
    fromMonadThrow (mkHistoryItem target commit buildUrl)

instance Csv.ToRecord HistoryItem where
  toRecord HistoryItem {..} = Csv.record
    [ Csv.toField historyItemTarget
    , Csv.toField historyItemSha1
    , Csv.toField (URI.render historyItemBuildUrl)
    ]

-- | Exception that is thrown when 'HistoryItem' cannot be constructed.

data HistoryItemException
  = InvalidHistoryItem !Text !Text !URI
  deriving (Eq, Ord, Show, Typeable)

instance Exception HistoryItemException

-- | Create a 'HistoryItem' value. The smart constructor makes sure that
--
--     * We'll be able to obtain 'reportPath' from it.
--     * We'll be able to botain 'logPath' from it.
--     * Target is a valid @'URI.RText' 'URI.PathPiece'@.
--     * GHC commit SHA1 is a valid @'URI.RText' 'URI.PathPiece'@.

mkHistoryItem :: MonadThrow m
  => Text              -- ^ Target name
  -> Text              -- ^ Commit SHA1
  -> URI               -- ^ Link to the build
  -> m HistoryItem
mkHistoryItem target sha1 buildUrl =
  maybe (throwM $ InvalidHistoryItem target sha1 buildUrl) return $ do
    let item = HistoryItem target sha1 buildUrl
    -- Assert that reportPath is obtainable
    void (reportPathMaybe item)
    -- Assert that logPath is obtainable
    void (logsPathMaybe item)
    -- Assert that target is a valid RText 'PathPiece
    void (URI.mkPathPiece target)
    -- Assert that commit is a valid RText 'PathPiece
    void (URI.mkPathPiece sha1)
    return item

-- | Get textual representation of a 'HistoryItem'.

hitemPretty :: HistoryItem -> Text
hitemPretty (HistoryItem target sha1 _) = target <> "-" <> sha1

-- | Convert 'HistoryItem' to a 'String'.

hitemPrettyStr :: HistoryItem -> String
hitemPrettyStr = T.unpack . hitemPretty

-- | Get name of file where corresponding report is stored.

reportPath :: HistoryItem -> Path Rel File
reportPath = fromJust . reportPathMaybe

-- | Construct report path with a possibility of failure.

reportPathMaybe :: HistoryItem -> Maybe (Path Rel File)
reportPathMaybe item = do
  p <- parseRelFile (hitemPrettyStr item)
  p <.> "csv"

-- | Get sub-directory that should contain build\/test logs for this
-- 'HistoryItem'.

logsPath :: HistoryItem -> Path Rel Dir
logsPath = fromJust . logsPathMaybe

-- | Construct log path with a possibility of failure.

logsPathMaybe :: HistoryItem -> Maybe (Path Rel Dir)
logsPathMaybe = parseRelDir . hitemPrettyStr

-- | Project target name.

hitemTarget :: HistoryItem -> Text
hitemTarget = historyItemTarget

-- | Project commit SHA1.

hitemCommit :: HistoryItem -> Text
hitemCommit = historyItemSha1

-- | Project build URL.

hitemBuildUrl :: HistoryItem -> URI
hitemBuildUrl = historyItemBuildUrl

----------------------------------------------------------------------------
-- Build history

-- | Build history.

newtype History = History (Vector HistoryItem)

-- | Load history from given history file. If the file does not exist empty
-- 'History' is returned.

loadHistory
  :: Path Abs File     -- ^ Location of history file
  -> IO (Either String History)
loadHistory path = do
  mhistory <- (forgivingAbsence . BL.readFile . fromAbsFile) path
  return $ case mhistory of
    Nothing    -> Right (History V.empty)
    Just bytes ->
      second History (Csv.decode Csv.NoHeader bytes)

-- | Save history to given history file.

saveHistory
  :: Path Abs File     -- ^ Location of history file
  -> History           -- ^ History to save
  -> IO ()
saveHistory path (History history) =
  BL.writeFile (fromAbsFile path) (Csv.encode (V.toList history))

-- | Append new item to the history.

extendHistory :: History -> HistoryItem -> History
extendHistory (History v) reportBasename =
  History (V.cons reportBasename v)

-- | Get newest history item.

newestHistoryItem :: History -> Maybe HistoryItem
newestHistoryItem (History v) = v V.!? 0

-- | Get two most recent history items (in order: older, newer). If there
-- are not enough history itmes, return 'Nothing'.

twoNewestHistoryItems :: History -> Maybe (HistoryItem, HistoryItem)
twoNewestHistoryItems (History v)
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

cabalConfigUrl :: HistoryItem -> URI
cabalConfigUrl item = [Q.uri|https://www.stackage.org|]
  { URI.uriPath = Just
    ( False
    , hitemTargetPiece item :| [ [Q.pathPiece|cabal.config|] ]
    )
  }
  where
    hitemTargetPiece = fromJust . URI.mkPathPiece . hitemTarget

-- | Produce URL at which Phabricator will show the given commit of interest.

ghcCommitUrl :: HistoryItem -> URI
ghcCommitUrl item = [Q.uri|https://phabricator.haskell.org|]
  { URI.uriPath = Just
    ( False
    , commit :| []
    )
  }
  where
    -- NOTE This operation is guaranteed to be safe.
    commit = fromJust $ URI.mkPathPiece ("rGHC" <> hitemCommit item)
