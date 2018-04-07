{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.Type
  ( BuildResults (..)
  , BuildStatus (..)
  , encodeBuildResults
  , decodeBuildResults
  , BuildInfo (..) )
where

import Control.Monad
import Data.Aeson
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Csv ((.!))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.HashMap.Strict  as HM
import qualified Data.Vector          as V

newtype BuildResults = BuildResults
  { unBuildResults :: HashMap Text BuildStatus
  } deriving (Show, Eq)

data BuildStatus
  = BuildSuccess !Int !Int
    -- ^ 'Int's are: the number of passing test suites and the number of
    -- failing test suites
  | BuildFailure
  | BuildUnreachable
  deriving (Show, Eq)

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

encodeBuildResults :: BuildResults -> BL.ByteString
encodeBuildResults
  = Csv.encode
  . fmap BuildItem
  . HM.toList
  . unBuildResults

decodeBuildResults :: BL.ByteString -> Either String BuildResults
decodeBuildResults
  = second (BuildResults .
            HM.fromList  .
            V.toList     .
            V.map unBuildItem)
  . Csv.decode Csv.NoHeader

-- | GHC build info as to be found in GHC metadata file.

data BuildInfo = BuildInfo
  { biSha1 :: !String
  -- NOTE extend this record as necessary
  }

instance FromJSON BuildInfo where
  parseJSON = withObject "GHC build metadata" $ \o ->
    BuildInfo <$> o .: "sha1"
