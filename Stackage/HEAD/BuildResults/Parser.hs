{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stackage.HEAD.BuildResults.Parser
  ( parseBuildLog
  , dropPackageVersion )
where

import Control.Monad
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Stackage.HEAD.BuildResults
import Stackage.HEAD.Package
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

-- | Drop package version.

dropPackageVersion
  :: Text              -- ^ Original package identifier
  -> PackageName       -- ^ Truncated package identifier
dropPackageVersion x = fromMaybe (PackageName x)
  (parseMaybe pPackageName x)

pBuildLog :: Parser BuildResults
pBuildLog
  = BuildResults
  . ($ HM.empty)
  -- NOTE It is important that we compose the functions in the right order,
  -- so functions "produced" first are applied first.
  . foldl' (\f g x -> g (f x)) id
  <$> manyTill pLine eof

pLine :: Parser
  (HashMap PackageName BuildStatus -> HashMap PackageName BuildStatus)
pLine = choice
  [ insertResult (BuildSuccess 0 0) <$> pBuildSuccess
    -- NOTE The most reliable way to track build failures is to assume
    -- failure once we see the message that indicates that we configure a
    -- certain package. If later we see Copying/registering message, we
    -- overwrite that, otherwise it indeed failed.
  , insertResult (BuildFailure 0) <$> pConfiguring
  , doBoth (modifyResult incBlockedBy) (insertResult BuildUnreachable)
      <$> pBuildUnreachable
  , modifyResult incTestSuites   <$> pTestRun
  , modifyResult incTestFailures <$> pTestFailure
  , id <$ takeWhileP Nothing (/= '\n') <* optional eol
  ]
  where
    insertResult status = \package ->
      HM.insert package status
    modifyResult f = \package m ->
      case HM.lookup package m of
        Just x  -> HM.insert package (f package x) m
        Nothing -> m
    incTestSuites package = \case
      BuildSuccess p b -> BuildSuccess (p + 1) b
      _ -> error $
        "Unexpected test build failure for: " ++ strPackageName package
    incTestFailures package = \case
      BuildSuccess p b -> BuildSuccess (p - 1) (b + 1)
      _ -> error $
        "Unexpected test failure for: " ++ strPackageName package
    incBlockedBy _ = \case
      BuildFailure n -> BuildFailure (n + 1)
      other -> other
    doBoth f g = \(a, b) -> (f a . g b)

pBuildSuccess :: Parser PackageName
pBuildSuccess = oneLine $ do
  lit "Copying/registering "
  packageName <- pPackageName
  pPendingFailures
  return packageName

pConfiguring :: Parser PackageName
pConfiguring = oneLine $ do
  lit "Configuring "
  packageName <- pPackageName
  pPendingFailures
  return packageName

pBuildUnreachable :: Parser (PackageName, PackageName)
pBuildUnreachable = oneLine $ do
  packageName <- pPackageName'
  lit ": DependencyFailed (PackageName \""
  blockedBy <- pPackageName'
  lit "\")"
  return (blockedBy, packageName)

pTestRun :: Parser PackageName
pTestRun = oneLine $ do
  lit "Test run "
  packageName <- pPackageName
  lit " ("
  void pPackageName'
  lit ")"
  pPendingFailures
  return packageName

pTestFailure :: Parser PackageName
pTestFailure = oneLine $ do
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

pPackageName :: Parser PackageName
pPackageName = do
  let ch = label "package name char" (satisfy isPackageNameChar)
  PackageName . T.pack <$> manyTill ch pVersionSuffix

pPackageName' :: Parser PackageName
pPackageName' = PackageName <$>
  takeWhile1P (Just "package name char") isPackageNameChar

pVersionSuffix :: Parser ()
pVersionSuffix = void . try $ do
  void (char '-')
  void $ sepBy1 pSkipInteger (char '.')
  notFollowedBy (satisfy isPackageNameChar)

pSkipInteger :: Parser ()
pSkipInteger = void $ takeWhile1P (Just "digit") isDigit

isPackageNameChar :: Char -> Bool
isPackageNameChar x = isAlphaNum x || x == '-'
