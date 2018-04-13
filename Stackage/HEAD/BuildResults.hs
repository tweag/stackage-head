{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildResults
  ( BuildResults (..)
  , BuildStatus (..)
  , encodeBuildResults
  , decodeBuildResults
  , failingPackages
  , unreachablePackages
  , succeedingPackages
  , brSize
  , brResetBlockedBy )
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

-- | Auxiliary definition.

newtype BuildItem = BuildItem
  { unBuildItem :: (Text, BuildStatus)
  }

instance Csv.ToRecord BuildItem where
  toRecord (BuildItem (packageName, status)) =
    case status of
      BuildSuccess p b -> g "s" p b
      BuildFailure _   -> g "f" 0 0
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
          "f" -> return (BuildFailure 0)
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

-- | Reset the numbers of packages a failing package blocks (for all failing
-- packages).

brResetBlockedBy :: BuildResults -> BuildResults
brResetBlockedBy = BuildResults . HM.map f . unBuildResults
  where
    f (BuildFailure _) = BuildFailure 0
    f other            = other
