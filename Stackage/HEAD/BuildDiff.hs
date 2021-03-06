{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildDiff
  ( BuildDiff
  , diffBuildResults
  , isEmptyDiff
  , buildDiffItems
  , partitionByInnocence
  , prettyPrintBuildDiff )
where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Stackage.HEAD.BuildResults
import Stackage.HEAD.History
import Stackage.HEAD.Package
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HE
import qualified Data.Text           as T

-- | Diff between two 'BuildResult's.

newtype BuildDiff = BuildDiff
  { unBuildDiff :: HashMap PackageName
      ( Maybe BuildStatus
      , Maybe BuildStatus )
  } deriving (Show)

-- | Construct a value of the 'BuildDiff' type.

diffBuildResults
  :: BuildResults      -- ^ Older state
  -> BuildResults      -- ^ Newer state
  -> BuildDiff
diffBuildResults (BuildResults old) (BuildResults new) =
  BuildDiff . HM.filter (uncurry isChanged) $ HM.unionWith
    combine
    (HM.map liftOld old)
    (HM.map liftNew new)
  where
    -- NOTE When just the number of the packages that a failing package
    -- blocks changes, we do not consider it a change.
    isChanged (Just (BuildFailure _)) (Just (BuildFailure _)) = False
    isChanged x y = x /= y
    combine (x, _) (_, y) = (x, y)
    liftOld x = (Just x, Nothing)
    liftNew x = (Nothing, Just x)

-- | Check if the diff is empty.

isEmptyDiff :: BuildDiff -> Bool
isEmptyDiff = HM.null . unBuildDiff

-- | Extract components of a diff.

buildDiffItems
  :: BuildDiff
  -> [(PackageName, ( Maybe BuildStatus
                    , Maybe BuildStatus ))]
buildDiffItems = sortBy (comparing fst) . HM.toList . unBuildDiff

-- | Obtain (in order) innocent part of a diff and suspicious part of a
-- diff.

partitionByInnocence
  :: HashSet PackageName -- ^ Packages with flaky test suites
  -> BuildDiff
  -> (BuildDiff, BuildDiff)
partitionByInnocence flaky (BuildDiff m) =
  ( BuildDiff innocent
  , BuildDiff suspicious )
  where
    innocent   = HM.difference m suspicious
    suspicious = HM.filterWithKey checkSuspicious m
    checkSuspicious packageName (x,y) =
      not (HE.member packageName flaky) && isChangeSuspicious x y

-- | Return 'True' if given progression of package build statuses is
-- suspicious and should be noticed by the GHC team.

isChangeSuspicious :: Maybe BuildStatus -> Maybe BuildStatus -> Bool
isChangeSuspicious Nothing Nothing  = True -- this can't be, so suspicious
isChangeSuspicious (Just _) Nothing = True -- a package disappeared, not good
isChangeSuspicious Nothing (Just _) = True -- also strange
isChangeSuspicious (Just old) (Just new) =
  case (old, new) of
    -- There is no change, so this case won't be evaluated. But let's
    -- consider it not suspicious (nothing changes after all).
    (BuildFailure _,   BuildFailure _)     -> False
    -- A package became unreachable, this is suspicious.
    (BuildFailure _,   BuildUnreachable) -> True
    -- Something was fixed, good. Still, we should look if any of its test
    -- suites fail.
    (BuildFailure _,   BuildSuccess _ b) -> b > 0
    -- New failure, always suspicious.
    (BuildUnreachable, BuildFailure _)   -> True
    -- There is no change, so this case won't be evaluated. Still
    -- unreachable, so not suspicious.
    (BuildUnreachable, BuildUnreachable) -> False
    -- This packages now builds, so really we should look if any of its test
    -- suites fail.
    (BuildUnreachable, BuildSuccess _ b) -> b > 0
    -- Now the package fails to build, suspicious.
    (BuildSuccess _ _, BuildFailure _)   -> True
    -- A package became unreachable, suspicious.
    (BuildSuccess _ _, BuildUnreachable) -> True
    -- Here we should look carefully at the results of running test suites.
    (BuildSuccess p0 b0, BuildSuccess p1 b1) ->
      let moreTestSuitesFailToBuild = (p0 + b0) > (p1 + b1)
          moreTestSuitesFail        = b1 > b0
      in moreTestSuitesFailToBuild || moreTestSuitesFail

-- | Pretty-print a 'BuildDiff'.

prettyPrintBuildDiff
  :: (HistoryItem, HistoryItem)
     -- ^ Corresponding older and newer history items
  -> BuildDiff         -- ^ The diff to render
  -> Text              -- ^ Rendition of the diff
prettyPrintBuildDiff (olderItem, newerItem) (BuildDiff m) =
  foldMap (\(packageName, (olderState, newerState)) ->
             prettyPrintChange packageName
                               (olderItem, olderState)
                               (newerItem, newerState))
          (sortBy (comparing fst) (HM.toList m))

-- | Pretty-print a change of single package.

prettyPrintChange
  :: PackageName
     -- ^ Package name
  -> (HistoryItem, Maybe BuildStatus)
     -- ^ Older history item and older status
  -> (HistoryItem, Maybe BuildStatus)
     -- ^ Newer history item and newer status
  -> Text
     -- ^ Rendition of the change
prettyPrintChange packageName (olderItem, olderStatus) (newerItem, newerStatus)
  = T.unlines
    [ unPackageName packageName <> ":"
    , ind <> "at " <> hitemPretty olderItem
    , ind2 <> prettyPrintStatus olderStatus
    , ind <> "at " <> hitemPretty newerItem
    , ind2 <> prettyPrintStatus newerStatus
    ]
  where
    ind  = "  "
    ind2 = "    "

-- | Pretty print a single 'BuildStatus'.

prettyPrintStatus :: Maybe BuildStatus -> Text
prettyPrintStatus Nothing  = "not present"
prettyPrintStatus (Just x) =
  case x of
    BuildFailure _   -> "build failure"
    BuildUnreachable -> "build unreachable"
    BuildSuccess p b -> "build succeeded, " <>
      showInt p <> " test suites passed, " <>
      showInt b <> " test suites failed"
  where
    showInt :: Int -> Text
    showInt = T.pack . show
