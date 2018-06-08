module Stackage.HEAD.BuildLogs
  ( copyPerPackageLogs
  , dropPerPackageLogs
  , retrieveBuildLog
  , retrieveTestLog )
where

import Control.Monad
import Data.IORef
import Data.Text (Text)
import Path.IO (forgivingAbsence, ignoringAbsence)
import Stackage.HEAD.BuildResults
import Stackage.HEAD.BuildResults.Parser
import Stackage.HEAD.History
import Stackage.HEAD.Package
import System.Directory
import System.FilePath
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

-- | Copy per-package logs to the directory containing build results. Later
-- we can request build logs using functions from this module.

copyPerPackageLogs
  :: FilePath          -- ^ Source directory
  -> FilePath          -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> BuildResults      -- ^ Build results corresponding to this history item
  -> IO Int            -- ^ Number of copied files
copyPerPackageLogs srcDir outputDir historyItem (BuildResults br) = do
  let f x = (dropPackageVersion x, PackageName x)
  m <- HM.fromList . fmap (f . T.pack) <$> listDirectory srcDir
  totalCopied <- newIORef 0
  forM_ (HM.keys br) $ \opackage ->
    forM_ (HM.lookup opackage m) $ \ipackage -> do
      let ip = strPackageName ipackage
          tpath = locatePackageLogs outputDir historyItem opackage
      createDirectoryIfMissing True tpath
      ignoringAbsence $ do
        copyFile (srcDir </> ip </> buildLog) (tpath  </> buildLog)
        modifyIORef' totalCopied (+ 1)
      ignoringAbsence $ do
        copyFile (srcDir </> ip </> testLog) (tpath  </> testLog)
        modifyIORef' totalCopied (+ 1)
  readIORef totalCopied

-- | Drop per-package build logs identified by the arguments.

dropPerPackageLogs
  :: FilePath          -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> IO ()
dropPerPackageLogs outputDir historyItem = ignoringAbsence $
  removeDirectoryRecursive (locateLogs outputDir historyItem)

-- | Get contents of build log (if present) for a package.

retrieveBuildLog
  :: FilePath          -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> PackageName       -- ^ Package of interest
  -> IO (Maybe Text)   -- ^ The log, if present
retrieveBuildLog outputDir historyItem packageName =
  forgivingAbsence . T.readFile $
    locatePackageLogs outputDir historyItem packageName </> buildLog

-- | Get contents of test log (if present) for a package.

retrieveTestLog
  :: FilePath          -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> PackageName       -- ^ Package of interest
  -> IO (Maybe Text)   -- ^ The log, if present
retrieveTestLog outputDir historyItem packageName =
  forgivingAbsence . T.readFile $
    locatePackageLogs outputDir historyItem packageName </> testLog

-- | Get directory prefix where logs should be located.

locatePackageLogs
  :: FilePath          -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> PackageName       -- ^ Package of interest
  -> FilePath          -- ^ Directory where logs should be
locatePackageLogs outputDir historyItem packageName =
  locateLogs outputDir historyItem </> strPackageName packageName

-- | Get directory prefix where logs should be located.

locateLogs
  :: FilePath          -- ^ Directory with build results
  -> HistoryItem       -- ^ History item the logs belong to
  -> FilePath          -- ^ Directory where per-package dirs should be
locateLogs outputDir historyItem =
  outputDir </> dropExtensions (strHistoryItem historyItem)

buildLog, testLog :: FilePath
buildLog = "build.out"
testLog  = "test.out"
