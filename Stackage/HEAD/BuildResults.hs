{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildResults
  ( BuildResults (..)
  , BuildStatus (..)
  , encodeBuildResults
  , decodeBuildResults )
where

import Control.Monad
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Csv ((.!))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.HashMap.Strict  as HM
import qualified Data.Vector          as V

-- | Results of building a build plan by Stackage curator typically obtained
-- through parsing of its log or by loading prepared build results file in
-- CSV format.

newtype BuildResults = BuildResults
  { unBuildResults :: HashMap Text BuildStatus
    -- ^ 'BuildStatus'es per package
  } deriving (Show, Eq)

-- | Build status.

data BuildStatus
  = BuildFailure
    -- ^ The package failed to build
  | BuildUnreachable
    -- ^ The package could not be built for some reason (e.g. its dependency
    -- failed to build—the most common case)
  | BuildSuccess !Int !Int
    -- ^ Success, 'Int's are:
    --     * the number of passing test suites
    --     * the number of failing test suites
  deriving (Show, Eq)

-- | Auxiliary definition.

newtype BuildItem = BuildItem
  { unBuildItem :: (Text, BuildStatus)
  }

instance Csv.ToRecord BuildItem where
  toRecord (BuildItem (packageName, status)) =
    case status of
      BuildSuccess p b -> g "s" p b
      BuildFailure     -> g "f" 0 0
      BuildUnreachable -> g "u" 0 0
    where
      g :: ByteString -> Int -> Int -> Csv.Record
      g tag p b = Csv.record
        [ Csv.toField packageName
        , Csv.toField tag
        , Csv.toField p
        , Csv.toField b
        ]

instance Csv.FromRecord BuildItem where
  parseRecord v
    | length v == 4 = do
        packageName <- v .! 0
        succeedingTests <- v .! 2
        failingTests <- v .! 3
        tag <- v .! 1
        buildStatus <- case tag of
          "s" -> return (BuildSuccess succeedingTests failingTests)
          "f" -> return BuildFailure
          "u" -> return BuildUnreachable
          _   -> fail ("unknown tag: \"" ++ tag ++ "\"")
        return $ BuildItem (packageName, buildStatus)
    | otherwise = mzero

-- | Construct binary representation of given 'BuildResult's value.

encodeBuildResults :: BuildResults -> BL.ByteString
encodeBuildResults
  = Csv.encode
  . fmap BuildItem
  . HM.toList
  . unBuildResults

-- | Restore 'BuildResult's from a 'BL.ByteString'.

decodeBuildResults :: BL.ByteString -> Either String BuildResults
decodeBuildResults
  = second (BuildResults .
            HM.fromList  .
            V.toList     .
            V.map unBuildItem)
  . Csv.decode Csv.NoHeader
