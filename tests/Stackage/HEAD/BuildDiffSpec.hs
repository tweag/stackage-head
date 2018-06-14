{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildDiffSpec
  ( spec )
where

import Control.Monad
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.BuildResults
import Stackage.HEAD.Package
import Test.Hspec
import Test.QuickCheck
import qualified Data.HashMap.Strict as HM

spec :: Spec
spec = do
  describe "diffBuildResults" $ do
    context "when both build results are empty" $
      it "the resulting diff is empty" $
        diffBuildResults emptyBr emptyBr `shouldSatisfy` isEmptyDiff
    context "when we have two build failures" $
      it "they are considered the same outcome" $
        property $ \x y -> do
          let oldStatus = BuildFailure (getNonNegative x)
              newStatus = BuildFailure (getNonNegative y)
          diffBuildResults
            (singletonBr pkgName oldStatus)
            (singletonBr pkgName newStatus)
            `shouldSatisfy` isEmptyDiff
    context "when we have a change in status" $
      it "is included in the diff" $
        property $ \oldStatus newStatus -> do
          when (presentFailure oldStatus && presentFailure newStatus)
            discard -- covered above
          when (oldStatus == newStatus)
            discard
          diffBuildResults
            (maybe emptyBr (singletonBr pkgName) oldStatus)
            (maybe emptyBr (singletonBr pkgName) newStatus)
            `shouldNotSatisfy` isEmptyDiff
    context "when status does not change" $
      it "is not included in the diff" $
        property $ \status -> do
          when (presentFailure (Just status)) discard -- covered above
          diffBuildResults
            (singletonBr pkgName status)
            (singletonBr pkgName status)
            `shouldSatisfy` isEmptyDiff

  -- TODO partitionByInnocence

----------------------------------------------------------------------------
-- Helpers

-- | Empty 'BuildResults' table.

emptyBr :: BuildResults
emptyBr = BuildResults HM.empty

-- | Construct a singleton 'BuildResults' table.

singletonBr :: PackageName -> BuildStatus -> BuildResults
singletonBr n s = BuildResults (HM.singleton n s)

-- | Dummy package name.

pkgName :: PackageName
pkgName = PackageName "my-package"

-- | “Present failure” predicate.

presentFailure :: Maybe BuildStatus -> Bool
presentFailure (Just (BuildFailure _)) = True
presentFailure _                       = False
