{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Data.Bifunctor (first)
import Data.HashSet (HashSet)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Data.Semigroup ((<>))
import Options.Applicative
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.BuildInfo
import Stackage.HEAD.BuildLogs
import Stackage.HEAD.BuildResults
import Stackage.HEAD.BuildResults.Parser
import Stackage.HEAD.History
import Stackage.HEAD.Package
import Stackage.HEAD.Utils (removeEither)
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import Text.URI (URI)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HE
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Path                 as P
import qualified Path.IO              as PIO
import qualified Stackage.HEAD.Site   as Site
import qualified Text.Megaparsec      as M
import qualified Text.URI             as URI

-- | Command line options.

data Options = Options
  { optCommand :: HashSet PackageName -> FilePath -> IO ()
    -- ^ Command
  , optFlakyPkgs :: HashSet PackageName
    -- ^ Flaky packages
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
           <*> (strOption . mconcat)
           [ long "ghc-metadata"
           , metavar "METADATA-FILE"
           , help "Location of GHC metadata file"
           ]
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
           <*> (strOption . mconcat)
           [ long "target"
           , metavar "TARGET"
           , help "Target name, without extension (lts-x.y, nightly-yyyy-mm-dd)"
           ])
     (progDesc "Add a new report to the history")) <>
   command "diff"
    (info (pure diffReports)
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
            ])
     (progDesc "Generate static web site using the data from build history")))
  <*> (toFlakySet <$> many ((strOption . mconcat)
    [ long "flaky"
    , metavar "PKG"
    , help "Flaky package, changes in its state should always be considered innocent"
    ]))
  <*> (strOption . mconcat)
    [ long "outdir"
    , metavar "OUTPUT-DIR"
    , help "Directory where to save resulting reports"
    ]

main :: IO ()
main = do
  Options {..} <- execParser optionsParserInfo
  optCommand optFlakyPkgs optOutputDir

-- | Generate a report from log file and add it to report history.

addReport
  :: URI               -- ^ Link to this CircleCI build
  -> FilePath          -- ^ Location of metadata JSON file
  -> FilePath          -- ^ Location of build log
  -> Maybe FilePath    -- ^ Location of per-package build logs
  -> String            -- ^ Target
  -> HashSet PackageName -- ^ Flaky packages
  -> FilePath          -- ^ Output directory containing build reports
  -> IO ()
addReport optBuildUrl optMetadata optBuildLog optPerPackageLogs optTarget _ optOutputDir = do
  BuildInfo {..} <- B.readFile optMetadata >>=
    removeEither . Aeson.eitherDecodeStrict'
  createDirectoryIfMissing True optOutputDir
  let reportPath = optOutputDir </> reportBasename
      reportBasename = (optTarget ++ "-" ++ biSha1) <.> "csv"
      actualItem = HistoryItem (T.pack reportBasename)
      historyPath = optOutputDir </> "history.csv"
      buildUrlPath = optOutputDir </> historyItemToBuildUrl actualItem
  putStrLn $ "Loading history file " ++ historyPath
  history <- loadHistory historyPath >>= removeEither
  when (presentInHistory history actualItem) $ do
    putStrLn $ "Report " ++ reportBasename
      ++ " is already present in history, so do nothing."
    exitSuccess
  putStrLn $ "Loading/parsing build log " ++ optBuildLog
  rawText <- TIO.readFile optBuildLog
  buildResults <- removeEither $ first
    (M.parseErrorPretty' rawText)
    (parseBuildLog optBuildLog rawText)
  putStrLn $ "Saving report to " ++ reportPath
  BL.writeFile reportPath (encodeBuildResults buildResults)
  putStrLn $ "Saving build URL to " ++ buildUrlPath
  TIO.writeFile buildUrlPath (URI.render optBuildUrl)
  forM_ optPerPackageLogs $ \srcDir -> do
    putStrLn "Copying per-package build logs"
    totalCopied <- copyPerPackageLogs
      srcDir optOutputDir actualItem buildResults
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
  putStrLn $ "Extending history file " ++ historyPath
  saveHistory historyPath (extendHistory history actualItem)

-- | Diff two latest items in the report history and detect suspicious
-- changes. Exit with non-zero status code if suspicious changes were
-- detected.

diffReports
  :: HashSet PackageName -- ^ Flaky packages
  -> FilePath          -- ^ Output directory containing build reports
  -> IO ()
diffReports flakyPkgs optOutputDir = do
  let historyPath = optOutputDir </> "history.csv"
  putStrLn $ "Loading history file " ++ historyPath
  history <- loadHistory historyPath >>= removeEither
  case twoLastHistoryItems history of
    Nothing -> do
      putStrLn "Not enough history items, so do nothing."
      exitSuccess
    Just (olderItem, newerItem) -> do
      let olderReportPath = optOutputDir
            </> strHistoryItem olderItem
          newerReportPath = optOutputDir
            </> strHistoryItem newerItem
      putStrLn "Going to compare:"
      putStrLn ("  older report " ++ olderReportPath)
      olderResults <- BL.readFile olderReportPath >>=
        removeEither . decodeBuildResults
      putStrLn ("  newer report " ++ newerReportPath)
      newerResults <- BL.readFile newerReportPath >>=
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
  -> HashSet PackageName -- ^ Flaky packages
  -> FilePath          -- ^ Output directory containing build reports
  -> IO ()
truncateHistory optHistoryLength _ optOutputDir = do
  let historyPath = optOutputDir </> "history.csv"
  putStrLn $ "Loading history file " ++ historyPath
  history <- loadHistory historyPath >>= removeEither
  let (newHistory, oldHistory) = splitHistory optHistoryLength history
  putStrLn $ "Saving history file " ++ historyPath
  saveHistory historyPath newHistory
  forM_ (historyItems oldHistory) $ \item -> do
    let reportPath = optOutputDir </> T.unpack (unHistoryItem item)
        buildUrlPath = optOutputDir </> historyItemToBuildUrl item
    putStrLn $ "Dropping old report " ++ reportPath
    removeFile reportPath
    putStrLn $ "Dropping URL file " ++ buildUrlPath
    removeFile buildUrlPath
    putStrLn $ "Dropping old per-build logs for that report"
    dropPerPackageLogs optOutputDir item

-- | Generate a static web site presenting build results and diffs in
-- readable form to the world.

generateSite
  :: FilePath          -- ^ Where to save the site
  -> HashSet PackageName -- ^ Flaky packages
  -> FilePath          -- ^ Output directory containing build reports
  -> IO ()
generateSite siteDirRaw flakyPkgs reportsDir' = do
  siteDir <- PIO.resolveDir' siteDirRaw
  reportsDir <- PIO.resolveDir' reportsDir'
  historyFile <- PIO.resolveFile reportsDir "history.csv"
  let params = Site.SiteParams
        { spLocation     = siteDir
        , spBuildReports = reportsDir
        , spHistoryFile  = historyFile
        , spFlakyPkgs    = flakyPkgs
        }
  putStrLn $ "Generating a static site in " ++ P.fromAbsDir siteDir
  Site.generateSite params
  putStrLn "Done."

----------------------------------------------------------------------------
-- Helpers

uriParser :: ReadM URI
uriParser = eitherReader $ \s ->
  case URI.mkURI (T.pack s) of
    Nothing -> Left "failed to parse URI"
    Just  x -> Right x

toFlakySet :: [String] -> HashSet PackageName
toFlakySet = HE.fromList . fmap (PackageName . T.pack)
