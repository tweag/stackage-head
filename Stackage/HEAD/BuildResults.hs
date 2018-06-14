{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildResults
  ( BuildResults (..)
  , BuildStatus (..)
  , buildResultsItems
  , encodeBuildResults
  , decodeBuildResults
  , failingPackages
  , unreachablePackages
  , succeedingPackages
  , brSize )
where

import Control.Monad
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Csv ((.!))
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy)
import Data.Ord (comparing)
import Stackage.HEAD.Package
import Test.QuickCheck
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.HashMap.Strict  as HM
import qualified Data.Vector          as V

-- | Results of building a build plan by Stackage curator typically obtained
-- through parsing of its log or by loading prepared build results file in
-- CSV format.

newtype BuildResults = BuildResults
  { unBuildResults :: HashMap PackageName BuildStatus
    -- ^ 'BuildStatus'es per package
  } deriving (Show, Eq)

-- | Build status.

data BuildStatus
  = BuildFailure !Int
    -- ^ The package failed to build, the 'Int' indicates how many other
    -- packages it made unreachable
  | BuildUnreachable
    -- ^ The package could not be built for some reason (e.g. its dependency
    -- failed to build—the most common case)
  | BuildSuccess !Int !Int
    -- ^ Success, 'Int's are:
    --     * the number of passing test suites
    --     * the number of failing test suites
  deriving (Show, Eq, Ord)

instance Arbitrary BuildStatus where
  arbitrary = oneof
    [ BuildFailure . getNonNegative <$> arbitrary
    , pure BuildUnreachable
    , BuildSuccess
      <$> (getNonNegative <$> arbitrary)
      <*> (getNonNegative <$> arbitrary)
    ]

-- | Extract components of 'BuildResults'.

buildResultsItems
  :: BuildResults
  -> [(PackageName, BuildStatus)]
buildResultsItems = sortBy (comparing fst) . HM.toList . unBuildResults

-- | Auxiliary definition.

newtype BuildItem = BuildItem
  { unBuildItem :: (PackageName, BuildStatus)
  }

instance Csv.ToRecord BuildItem where
  toRecord (BuildItem (packageName, status)) =
    case status of
      BuildSuccess p b -> g "s" p b
      BuildFailure n   -> g "f" n 0
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
        col2 <- v .! 2
        col3 <- v .! 3
        tag  <- v .! 1
        buildStatus <- case tag of
          "s" -> return (BuildSuccess col2 col3)
          "f" -> return (BuildFailure col2)
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

-- | Return a subset of 'BuildResults' containing failing packages.

failingPackages :: BuildResults -> BuildResults
failingPackages = selectMatching $ \case
  BuildFailure _ -> True
  _              -> False

-- | Return a subset of 'BuildResults' containing unreachable packages.

unreachablePackages :: BuildResults -> BuildResults
unreachablePackages = selectMatching $ \case
  BuildUnreachable -> True
  _                -> False

-- | Return a subset of 'BuildResults' containing succeeding packages.

succeedingPackages :: BuildResults -> BuildResults
succeedingPackages = selectMatching $ \case
  BuildSuccess _ _ -> True
  _                -> False

selectMatching :: (BuildStatus -> Bool) -> BuildResults -> BuildResults
selectMatching f = BuildResults . HM.filter f . unBuildResults

-- | Return number of packages in 'BuildResults'.

brSize :: BuildResults -> Int
brSize = HM.size . unBuildResults
