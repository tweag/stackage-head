module Stackage.HEAD.BuildResults.ParserSpec
  ( spec )
where

import Stackage.HEAD.BuildResults
import Stackage.HEAD.BuildResults.Parser
import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO         as TIO
import qualified Text.Megaparsec      as M

spec :: Spec
spec = do
  describe "parseBuildLog" $ do
    withFiles "data/example-log-0.txt" "data/example-report-0.csv"
    withFiles "data/example-log-1.txt" "data/example-report-1.csv"
    withFiles "data/example-log-2.txt" "data/example-report-2.csv"

withFiles :: FilePath -> FilePath -> Spec
withFiles logPath reportPath =
  it ("transforms " ++ logPath ++ " into " ++ reportPath) $ do
    rawText <- TIO.readFile logPath
    case parseBuildLog logPath rawText of
      Left err -> expectationFailure $
        "could not parse build log:\n" ++ M.parseErrorPretty' rawText err
      Right actualResults -> do
        rawBytes <- BL.readFile reportPath
        case decodeBuildResults rawBytes of
          Left err -> expectationFailure $
            "could not load expected build results:\n" ++ err
          Right expectedResults -> do
            actualResults `shouldBe` expectedResults
