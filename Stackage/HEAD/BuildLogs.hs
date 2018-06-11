{-# LANGUAGE TemplateHaskell #-}

module Stackage.HEAD.BuildLogs
  ( copyPerPackageLogs
  , dropPerPackageLogs
  , retrieveBuildLog
  , retrieveTestLog )
where

import Control.Monad
import Data.IORef
import Data.Text (Text)
import Path
import Path.IO
import Stackage.HEAD.BuildResults
import Stackage.HEAD.BuildResults.Parser
import Stackage.HEAD.History
import Stackage.HEAD.Package
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

-- | Copy per-package logs to the directory containing build results. Later
-- we can request build logs using functions from this module.

copyPerPackageLogs
  :: Path Abs Dir      -- ^ Source directory
  -> Path Abs Dir      -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> BuildResults      -- ^ Build results corresponding to this history item
  -> IO Int            -- ^ Number of copied files
copyPerPackageLogs srcDir outputDir historyItem (BuildResults br) = do
  let f x = (dropPackageVersion x, PackageName x)
  m <- HM.fromList . fmap (f . T.pack . fromRelFile . filename) . snd
    <$> listDir srcDir
  totalCopied <- newIORef 0
  forM_ (HM.keys br) $ \opackage ->
    forM_ (HM.lookup opackage m) $ \ipackage -> do
      let ip = packageNameRelDir ipackage
          tpath = locatePackageLogs outputDir historyItem opackage
      ensureDir tpath
      ignoringAbsence $ do
        copyFile (srcDir </> ip </> buildLog) (tpath  </> buildLog)
        modifyIORef' totalCopied (+ 1)
      ignoringAbsence $ do
        copyFile (srcDir </> ip </> testLog) (tpath  </> testLog)
        modifyIORef' totalCopied (+ 1)
  readIORef totalCopied

-- | Drop per-package build logs identified by the arguments.

dropPerPackageLogs
  :: Path Abs Dir      -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> IO ()
dropPerPackageLogs outputDir historyItem = ignoringAbsence $
  removeDirRecur (locateLogs outputDir historyItem)

-- | Get contents of build log (if present) for a package.

retrieveBuildLog
  :: Path Abs Dir      -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> PackageName       -- ^ Package of interest
  -> IO (Maybe Text)   -- ^ The log, if present
retrieveBuildLog outputDir historyItem packageName =
  forgivingAbsence . T.readFile . fromAbsFile $
    locatePackageLogs outputDir historyItem packageName </> buildLog

-- | Get contents of test log (if present) for a package.

retrieveTestLog
  :: Path Abs Dir      -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> PackageName       -- ^ Package of interest
  -> IO (Maybe Text)   -- ^ The log, if present
retrieveTestLog outputDir historyItem packageName =
  forgivingAbsence . T.readFile . fromAbsFile $
    locatePackageLogs outputDir historyItem packageName </> testLog

-- | Get directory prefix where logs should be located.

locatePackageLogs
  :: Path Abs Dir      -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> PackageName       -- ^ Package of interest
  -> Path Abs Dir      -- ^ Directory where logs should be
locatePackageLogs outputDir historyItem packageName =
  locateLogs outputDir historyItem </> packageNameRelDir packageName

-- | Get directory prefix where logs should be located.

locateLogs
  :: Path Abs Dir      -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> Path Abs Dir      -- ^ Directory where per-package dirs should be
locateLogs outputDir historyItem =
  outputDir </> logsPath historyItem

buildLog, testLog :: Path Rel File
buildLog = $(mkRelFile "build.out")
testLog  = $(mkRelFile "test.out")
