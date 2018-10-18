{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Data.Bifunctor (first)
import Data.HashSet (HashSet)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Options.Applicative
import Path
import Path.IO
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.BuildInfo
import Stackage.HEAD.BuildLogs
import Stackage.HEAD.BuildResults
import Stackage.HEAD.BuildResults.Parser
import Stackage.HEAD.History
import Stackage.HEAD.Package
import Stackage.HEAD.Utils (removeEither)
import System.Exit (exitFailure, exitSuccess)
import Text.URI (URI)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HE
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Stackage.HEAD.Site   as Site
import qualified Text.Megaparsec      as M
import qualified Text.URI             as URI

-- | Command line options.

data Options = Options
  { optCommand :: Path Abs Dir -> IO ()
    -- ^ Command
  , optOutputDir :: FilePath
    -- ^ Output directory
  }

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser) . mconcat $
  [ fullDesc
  , progDesc "Analysis and management of results of Stackage builds"
  , header "stackage-head"
  ]

optionsParser :: Parser Options
optionsParser = Options
  <$> hsubparser
  (command "add"
   (info (addReport
           <$> (option uriParser . mconcat)
            [ long "build-url"
            , metavar "URL"
            , help "Link to this CircleCI build"
            ]
           <*> ghcMetadataOpt
           <*> (strOption . mconcat)
           [ long "build-log"
           , metavar "LOG-FILE"
           , help "Location of Stackage curator build log"
           ]
           <*> (optional . strOption . mconcat)
           [ long "build-per-package-dir"
           , metavar "LOG-DIR"
           , help "Location of directory with per-package build logs"
           ]
           <*> targetOpt)
     (progDesc "Add a new report to the history")) <>
   command "diff"
    (info (diffReports
             <$> flakyPkgsOpt)
      (progDesc "Diff two latest history items and detect suspicious changes")) <>
   command "truncate"
    (info (truncateHistory
            <$> (option auto . mconcat)
            [ long "history-length"
            , metavar "N"
            , help "This many history items should be preserved"
            ])
     (progDesc "Truncate history and remove build reports that are too old")) <>
   command "generate-site"
    (info (generateSite
            <$> (strOption . mconcat)
            [ long "site-dir"
            , metavar "SITE-DIR"
            , help "Where to save the site"
            ]
            <*> flakyPkgsOpt)
     (progDesc "Generate static web site using the data from build history")) <>
   command "already-seen"
     (info (alreadySeen
             <$> ghcMetadataOpt
             <*> targetOpt)
       (progDesc "Return exit code 1 if given target/commit combo is already newest in history.")))
  <*> (strOption . mconcat)
    [ long "outdir"
    , metavar "OUTPUT-DIR"
    , help "Directory where to save resulting reports"
    ]
  where
    ghcMetadataOpt = (strOption . mconcat)
      [ long "ghc-metadata"
      , metavar "METADATA-FILE"
      , help "Location of GHC metadata file"
      ]
    targetOpt = (fmap T.pack . strOption . mconcat)
      [ long "target"
      , metavar "TARGET"
      , help "Target name, without extension (lts-x.y, nightly-yyyy-mm-dd)"
      ]
    flakyPkgsOpt = toPackageNameSet <$> many ((strOption . mconcat)
      [ long "flaky"
      , metavar "PKG"
      , help "Flaky package, changes in its state should always be considered innocent"
      ])

main :: IO ()
main = do
  Options {..} <- execParser optionsParserInfo
  resolveDir' optOutputDir >>= optCommand

-- | Generate a report from log file and add it to report history.

addReport
  :: URI               -- ^ Link to this CircleCI build
  -> FilePath          -- ^ Location of metadata JSON file
  -> FilePath          -- ^ Location of build log
  -> Maybe FilePath    -- ^ Location of per-package build logs
  -> Text              -- ^ Target
  -> Path Abs Dir      -- ^ Output directory containing build reports
  -> IO ()
addReport buildUrl metadataPath optBuildLog optPerPackageLogs target outputDir = do
  BuildInfo {..} <- B.readFile metadataPath >>=
    removeEither . Aeson.eitherDecodeStrict'
  ensureDir outputDir
  utcTime <- getCurrentTime
  item <- mkHistoryItem target biSha1 buildUrl utcTime
  let rpath = outputDir </> reportPath item
      hpath = historyPath outputDir
      lpath = latestBuildPath outputDir
  putStrLn $ "Loading history file " ++ fromAbsFile hpath
  history <- loadHistory hpath >>= removeEither
  when (Just item == newestHistoryItem history) $ do
    putStrLn $ "Report " ++ fromAbsFile rpath
      ++ " is already present in history, so do nothing."
    exitSuccess
  putStrLn $ "Loading/parsing build log " ++ optBuildLog
  rawText <- TIO.readFile optBuildLog
  buildResults <- removeEither $ first
    (M.parseErrorPretty' rawText)
    (parseBuildLog optBuildLog rawText)
  let buildResultsBs = encodeBuildResults buildResults
  forM_ [rpath, lpath] $ \p -> do
    putStrLn $ "Saving report to " ++ fromAbsFile p
    BL.writeFile (fromAbsFile p) buildResultsBs
  forM_ optPerPackageLogs $ \srcDirRaw -> do
    putStrLn "Copying per-package build logs"
    srcDir <- resolveDir' srcDirRaw
    totalCopied <- copyPerPackageLogs
      srcDir outputDir item buildResults
    putStrLn $ "Total files copied: " ++ show totalCopied
  let fpackages = failingPackages buildResults
      fpackagesSize = brSize fpackages
  putStrLn $ "  Failing packages: " ++
    show fpackagesSize
  putStrLn $ "  Unreachable packages: " ++
    (show . brSize . unreachablePackages) buildResults
  putStrLn $ "  Packages that build: " ++
    (show . brSize . succeedingPackages) buildResults
  when (fpackagesSize > 0) $ do
    putStrLn "Failing packages are the following:"
    let xs = sortBy (comparing (Down . snd)) . HM.toList $ unBuildResults fpackages
    forM_ xs $ \(packageName, status) -> do
      let n = case status of
                BuildFailure x -> x
                _              -> 0
      putStrLn $ "  - " ++ strPackageName packageName
        ++ ", blocking " ++ show n ++ " packages"
  putStrLn $ "Extending history file " ++ fromAbsFile hpath
  saveHistory hpath (extendHistory history item)

-- | Diff two latest items in the report history and detect suspicious
-- changes. Exit with non-zero status code if suspicious changes were
-- detected.

diffReports
  :: HashSet PackageName -- ^ Flaky packages
  -> Path Abs Dir        -- ^ Output directory containing build reports
  -> IO ()
diffReports flakyPkgs outputDir = do
  let hpath = historyPath outputDir
  putStrLn $ "Loading history file " ++ fromAbsFile hpath
  history <- loadHistory hpath >>= removeEither
  case twoNewestHistoryItems history of
    Nothing -> do
      putStrLn "Not enough history items, so do nothing."
      exitSuccess
    Just (olderItem, newerItem) -> do
      let olderReportPath = outputDir </> reportPath olderItem
          newerReportPath = outputDir </> reportPath newerItem
      putStrLn "Going to compare:"
      putStrLn ("  older report " ++ fromAbsFile olderReportPath)
      olderResults <- BL.readFile (fromAbsFile olderReportPath) >>=
        removeEither . decodeBuildResults
      putStrLn ("  newer report " ++ fromAbsFile newerReportPath)
      newerResults <- BL.readFile (fromAbsFile newerReportPath) >>=
        removeEither . decodeBuildResults
      let diff = diffBuildResults olderResults newerResults
          (innocentDiff, suspiciousDiff) = partitionByInnocence flakyPkgs diff
      when (isEmptyDiff diff) $ do
        putStrLn "No changes detected, nothing to do."
        exitSuccess
      unless (isEmptyDiff suspiciousDiff) $ do
        putStrLn "\n==== SUSPICIOUS CHANGES\n"
        TIO.putStrLn $ prettyPrintBuildDiff
          (olderItem, newerItem)
          suspiciousDiff
      unless (isEmptyDiff innocentDiff) $ do
        putStrLn "\n==== INNOCENT CHANGES\n"
        TIO.putStrLn $ prettyPrintBuildDiff
          (olderItem, newerItem)
          innocentDiff
      if isEmptyDiff suspiciousDiff
        then do putStrLn "No changes need attention of GHC team.\n"
                exitSuccess :: IO ()
        else do putStrLn "There are changes that need attention of GHC team.\n"
                exitFailure

-- | Truncate history leaving only N most recent history items.

truncateHistory
  :: Int               -- ^ How many history items to leave
  -> Path Abs Dir      -- ^ Output directory containing build reports
  -> IO ()
truncateHistory optHistoryLength outputDir = do
  let hpath = historyPath outputDir
  putStrLn $ "Loading history file " ++ fromAbsFile hpath
  history <- loadHistory hpath >>= removeEither
  let (newHistory, oldHistory) = splitHistory optHistoryLength history
  putStrLn $ "Saving history file " ++ fromAbsFile hpath
  saveHistory hpath newHistory
  forM_ (historyItems oldHistory) $ \item -> do
    let rpath = outputDir </> reportPath item
    putStrLn $ "Dropping old report " ++ fromAbsFile rpath
    ignoringAbsence (removeFile rpath)
    putStrLn "Dropping old per-build logs for that report"
    dropPerPackageLogs outputDir item

-- | Generate a static web site presenting build results and diffs in
-- readable form to the world.

generateSite
  :: FilePath          -- ^ Where to save the site
  -> HashSet PackageName -- ^ Flaky packages
  -> Path Abs Dir      -- ^ Output directory containing build reports
  -> IO ()
generateSite siteDirRaw flakyPkgs reportsDir = do
  siteDir <- resolveDir' siteDirRaw
  let params = Site.SiteParams
        { spLocation     = siteDir
        , spBuildReports = reportsDir
        , spHistoryFile  = historyPath reportsDir
        , spFlakyPkgs    = flakyPkgs
        }
  putStrLn $ "Generating a static site in " ++ fromAbsDir siteDir
  Site.generateSite params
  putStrLn "Done."

-- | Check whether the newest target\/commit combo matches the given one.
-- (If so, it makes sense for the CI script to skip building a build plan
-- again).

alreadySeen
  :: FilePath          -- ^ Location of metadata JSON file
  -> Text              -- ^ Target
  -> Path Abs Dir      -- ^ Output directory containing build reports
  -> IO ()
alreadySeen optMetadata target outputDir = do
  BuildInfo {..} <- B.readFile optMetadata >>=
    removeEither . Aeson.eitherDecodeStrict'
  item <- newestHistoryItem <$>
    (loadHistory (historyPath outputDir) >>= removeEither)
  if (hitemTarget <$> item) == Just target &&
     (hitemCommit <$> item) == Just biSha1
    then do
         putStrLn "The combination of target and commit is already in history"
         exitFailure
    else do
         putStrLn "The combination of target and commit is new to me"
         exitSuccess

----------------------------------------------------------------------------
-- Helpers

historyPath :: Path Abs Dir -> Path Abs File
historyPath outputDir = outputDir </> $(mkRelFile "history.csv")

latestBuildPath :: Path Abs Dir -> Path Abs File
latestBuildPath outputDir = outputDir </> $(mkRelFile "latest.csv")

uriParser :: ReadM URI
uriParser = eitherReader $ \s ->
  case URI.mkURI (T.pack s) of
    Nothing -> Left "failed to parse URI"
    Just  x -> Right x

toPackageNameSet :: [String] -> HashSet PackageName
toPackageNameSet = HE.fromList . fmap (PackageName . T.pack)
