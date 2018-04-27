{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildResults.ParserSpec
  ( spec )
where

import Stackage.HEAD.BuildResults
import Stackage.HEAD.BuildResults.Parser
import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text.IO         as TIO
import qualified Text.Megaparsec      as M

spec :: Spec
spec = do
  describe "parseBuildLog" $ do
    withFiles "data/example-log-0.txt" "data/example-report-0.csv"
    withFiles "data/example-log-1.txt" "data/example-report-1.csv"
    withFiles "data/example-log-2.txt" "data/example-report-2.csv"
    it "detects how many packages a failing package blocks" $ do
      BuildResults m <- loadLog "data/example-log-1.txt"
      HM.lookup "doctest"       m `shouldBe` Just (BuildFailure 84)
      HM.lookup "cabal-doctest" m `shouldBe` Just (BuildFailure 50)

withFiles :: FilePath -> FilePath -> Spec
withFiles logPath reportPath =
  it ("transforms " ++ logPath ++ " into " ++ reportPath) $ do
    actualResults   <- loadLog logPath
    expectedResults <- loadBuildResults reportPath
    actualResults `shouldBe` expectedResults

----------------------------------------------------------------------------
-- Helpers

loadLog :: FilePath -> IO BuildResults
loadLog path = do
  rawText <- TIO.readFile path
  case parseBuildLog path rawText of
    Left err -> do
      expectationFailure $
        "could not parse build log:\n" ++ M.parseErrorPretty' rawText err
      undefined -- expectationFailure should be IO a
    Right buildResults ->  return buildResults

loadBuildResults :: FilePath -> IO BuildResults
loadBuildResults path = do
  rawBytes <- BL.readFile path
  case decodeBuildResults rawBytes of
    Left err -> do
      expectationFailure $
        "could not load expected build results:\n" ++ err
      undefined
    Right buildResults -> return buildResults
