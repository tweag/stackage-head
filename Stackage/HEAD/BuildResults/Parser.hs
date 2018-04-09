{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildResults.Parser
  ( parseBuildLog )
where

import Control.Monad
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Data.Text (Text)
import Data.Void (Void)
import Stackage.HEAD.BuildResults
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

type Parser = Parsec Void Text

-- | Parse a build log produced by Stackage curator and turn it into
-- 'BuildResults'.

parseBuildLog
  :: FilePath          -- ^ Name of file
  -> Text              -- ^ Contents of file
  -> Either (ParseError Char Void) BuildResults
parseBuildLog = parse pBuildLog

pBuildLog :: Parser BuildResults
pBuildLog
  = BuildResults
  . ($ HM.empty)
  -- NOTE It is important that we compose the functions in the right order,
  -- so functions "produced" first are applied first.
  . foldl' (\f g x -> g (f x)) id
  <$> manyTill pLine eof

pLine :: Parser (HashMap Text BuildStatus -> HashMap Text BuildStatus)
pLine = choice
  [ insertResult (BuildSuccess 0 0) pBuildSuccess
    -- NOTE The most reliable way to track build failures is to assume
    -- failure once we see the message that indicates that the build has
    -- started. If later we see Copying/registering message, we overwrite
    -- that, otherwise it indeed failed.
  , insertResult BuildFailure pBuildStarted
  , insertResult BuildUnreachable pBuildUnreachable
  , modifyResult incTestSuites pTestRun
  , modifyResult incTestFailures pTestFailure
  , id <$ takeWhileP Nothing (/= '\n') <* optional eol
  ]
  where
    insertResult status = fmap $ \package ->
      HM.insert package status
    modifyResult f = fmap $ \package m ->
      case HM.lookup package m of
        Just x ->
          HM.insert package (f package x) m
        Nothing -> error $
          "Action on component of " ++ T.unpack package ++ " but it hasn't been built yet!"
    incTestSuites package = \case
      BuildSuccess p b -> BuildSuccess (p + 1) b
      _ -> error $
        "Unexpected test build failure for: " ++ T.unpack package
    incTestFailures package = \case
      BuildSuccess p b -> BuildSuccess (p - 1) (b + 1)
      _ -> error $
        "Unexpected test failure for: " ++ T.unpack package

pBuildSuccess :: Parser Text
pBuildSuccess = oneLine $ do
  lit "Copying/registering "
  packageName <- pPackageName
  pPendingFailures
  return packageName

pBuildStarted :: Parser Text
pBuildStarted = oneLine $ do
  lit "Building "
  packageName <- pPackageName
  pPendingFailures
  return packageName

pBuildUnreachable :: Parser Text
pBuildUnreachable = oneLine $ do
  packageName <- pPackageName'
  lit ": DependencyFailed (PackageName \""
  void pPackageName'
  lit "\")"
  return packageName

pTestRun :: Parser Text
pTestRun = oneLine $ do
  lit "Test run "
  packageName <- pPackageName
  lit " ("
  void pPackageName'
  lit ")"
  pPendingFailures
  return packageName

pTestFailure :: Parser Text
pTestFailure = try $ do
  packageName <- pPackageName'
  lit ": BuildFailureException Process exited with ExitFailure "
  pSkipInteger
  lit ": dist/build/"
  void pPackageName'
  lit "/"
  void pPackageName'
  return packageName

----------------------------------------------------------------------------
-- Helpers

lit :: Text -> Parser ()
lit = void . string

oneLine :: Parser a -> Parser a
oneLine p = do
  r <- try p
  r <$ optional eol

pPendingFailures :: Parser ()
pPendingFailures = do
  lit " (pending: "
  pSkipInteger
  lit ", failures: "
  pSkipInteger
  lit ")"

pPackageName :: Parser Text
pPackageName = do
  let ch = label "package name char" (satisfy isPackageNameChar)
  T.pack <$> manyTill ch pVersionSuffix

pPackageName' :: Parser Text
pPackageName' =
  takeWhile1P (Just "package name char") isPackageNameChar

pVersionSuffix :: Parser ()
pVersionSuffix = void . try $ do
  void (char '-')
  sepBy1 pSkipInteger (char '.')

pSkipInteger :: Parser ()
pSkipInteger = void $ takeWhile1P (Just "digit") isDigit

isPackageNameChar :: Char -> Bool
isPackageNameChar x = isAlphaNum x || x == '-'
