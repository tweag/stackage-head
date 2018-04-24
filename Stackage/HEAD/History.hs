module Stackage.HEAD.History
  ( History
  , HistoryItem (..)
  , strHistoryItem
  , loadHistory
  , saveHistory
  , presentInHistory
  , extendHistory
  , twoLastHistoryItems
  , splitHistory
  , historyItems )
where

import Data.Bifunctor (second)
import Data.Text (Text)
import Data.Vector (Vector)
import Stackage.HEAD.Utils
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Vector          as V

-- | Build history.

newtype History = History (Vector HistoryItem)

-- | Build history item.

newtype HistoryItem = HistoryItem
  { unHistoryItem :: Text
  } deriving (Eq)

instance Csv.FromRecord HistoryItem where
  parseRecord v = do
    bs <- v Csv..! 0
    (return . HistoryItem . TE.decodeUtf8) bs

instance Csv.ToRecord HistoryItem where
  toRecord (HistoryItem txt) =
    Csv.record [TE.encodeUtf8 txt]

-- | Convert 'HistoryItem' to a 'String'.

strHistoryItem :: HistoryItem -> String
strHistoryItem = T.unpack . unHistoryItem

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
