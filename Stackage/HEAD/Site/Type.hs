{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Stackage.HEAD.Site.Type
  ( SiteParams (..)
  , DiffTable
  , Location
  , mkLocation
  , location
  , locationPath
  , locationUri )
where

import Control.Monad
import Control.Monad.Catch
import Data.Data (Data)
import Data.HashSet (HashSet)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Language.Haskell.TH hiding (location)
import Language.Haskell.TH.Syntax (lift, dataToExpQ)
import Path
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.History
import Stackage.HEAD.Package
import Text.URI
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import qualified Text.URI           as URI

-- | Inputs for the site generator.

data SiteParams = SiteParams
  { spLocation     :: !(Path Abs Dir)
  , spBuildReports :: !(Path Abs Dir)
  , spHistoryFile  :: !(Path Abs File)
  , spFlakyPkgs :: !(HashSet PackageName)
  }

-- | A map from newer history item in diff to older history item and the
-- actual diff partitioned by innocence: first innocent diff and suspicious
-- diff.

type DiffTable = Map HistoryItem (HistoryItem, (BuildDiff, BuildDiff))

-- | Location of a page or resource on the site.

newtype Location = Location
  { unLocation :: NonEmpty (RText 'PathPiece)
  } deriving (Typeable, Data)

instance Semigroup Location where
  Location x <> Location y = Location (x <> y)

-- | Construction of 'Location' may fail, but then we can ensure that its
-- conversion to 'Path' and 'URI' cannot.

mkLocation :: MonadThrow m => NonEmpty Text -> m Location
mkLocation pieces = do
  loc <- Location <$> forM pieces URI.mkPathPiece
  loc <$ locationPathThrow loc

-- | Compile time validation of 'Location' values.

location
  :: [Text]            -- ^ Plain list is just easier to type
  -> Q Exp
location pieces =
  case NE.nonEmpty pieces of
    Nothing -> fail "the input collection should not be empty"
    Just ps -> mkLocation ps >>= dataToExpQ (fmap liftText . cast)
  where
    liftText :: Text -> Q Exp
    liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

-- | Interpret 'Location' as a relative path.

locationPath :: Location -> Path Rel File
locationPath = fromJust . locationPathThrow

-- | Like 'locationPath' but potentially can throw.

locationPathThrow :: MonadThrow m => Location -> m (Path Rel File)
locationPathThrow
  = parseRelFile
  . intercalate "/"
  . fmap (T.unpack . unRText)
  . NE.toList
  . unLocation

-- | Interpret 'Location' as a 'URI'.

locationUri :: Location -> URI
locationUri (Location pieces) = emptyURI
  { uriAuthority = Left True
  , uriPath = Just (False, pieces) }
