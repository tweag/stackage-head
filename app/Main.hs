{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Data.Bifunctor (first)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Data.Semigroup ((<>))
import Options.Applicative
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.BuildInfo
import Stackage.HEAD.BuildResults
import Stackage.HEAD.BuildResults.Parser
import Stackage.HEAD.History
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Text.Megaparsec      as M

-- | Command line options.

data Options = Options
  { optCommand :: FilePath -> IO ()
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
           <$> (strOption . mconcat)
           [ long "ghc-metadata"
           , metavar "METADATA-FILE"
           , help "Location of GHC metadata file."
           ]
           <*> (strOption . mconcat)
           [ long "build-log"
           , metavar "LOG-FILE"
           , help "Location of Stackage curator build log."
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
     (progDesc "Truncate history and remove build reports that are too old")))

  <*> (strOption . mconcat)
    [ long "outdir"
    , metavar "OUTPUT-DIR"
    , help "Directory where to save resulting reports."
    ]

main :: IO ()
main = do
  Options {..} <- execParser optionsParserInfo
  optCommand optOutputDir

-- | Generate a report from log file and add it to report history.

addReport
  :: FilePath          -- ^ Location of metadata JSON file
  -> FilePath          -- ^ Location of build log
  -> String            -- ^ Target
  -> FilePath          -- ^ Output directory containing build reports
  -> IO ()
addReport optMetadata optBuildLog optTarget optOutputDir = do
  BuildInfo {..} <- B.readFile optMetadata >>=
    removeEither . Aeson.eitherDecodeStrict'
  createDirectoryIfMissing True optOutputDir
  let reportPath = optOutputDir </> reportBasename
      reportBasename = (optTarget ++ "-" ++ biSha1) <.> "csv"
      actualItem = HistoryItem (T.pack reportBasename)
      historyPath = optOutputDir </> "history.csv"
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
      putStrLn $ "  - " ++ T.unpack packageName
        ++ ", blocking " ++ show n ++ " packages"
  putStrLn $ "Extending history file " ++ historyPath
  saveHistory historyPath (extendHistory history actualItem)

-- | Diff two latest items in the report history and detect suspicious
-- changes. Exit with non-zero status code if suspicious changes were
-- detected.

diffReports
  :: FilePath          -- ^ Output directory containing build reports
  -> IO ()
diffReports optOutputDir = do
  let historyPath = optOutputDir </> "history.csv"
  putStrLn $ "Loading history file " ++ historyPath
  history <- loadHistory historyPath >>= removeEither
  case twoLastHistoryItems history of
    Nothing -> do
      putStrLn "Not enough history items, so do nothing."
      exitSuccess
    Just (olderItem, newerItem) -> do
      let olderReportPath = optOutputDir
            </> T.unpack (unHistoryItem olderItem)
          newerReportPath = optOutputDir
            </> T.unpack (unHistoryItem newerItem)
      putStrLn "Going to compare:"
      putStrLn ("  older report " ++ olderReportPath)
      olderResults <- BL.readFile olderReportPath >>=
        removeEither . decodeBuildResults
      putStrLn ("  newer report " ++ newerReportPath)
      newerResults <- BL.readFile newerReportPath >>=
        removeEither . decodeBuildResults
      let diff = diffBuildResults olderResults newerResults
          (innocentDiff, suspiciousDiff) = partitionByInnocence diff
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
                exitSuccess
        else do putStrLn "There are changes that need attention of GHC team.\n"
                exitFailure

-- | Truncate history leaving only N most recent history items.

truncateHistory
  :: Int               -- ^ How many history items to leave
  -> FilePath          -- ^ Output directory containing build reports
  -> IO ()
truncateHistory optHistoryLength optOutputDir = do
  let historyPath = optOutputDir </> "history.csv"
  putStrLn $ "Loading history file " ++ historyPath
  history <- loadHistory historyPath >>= removeEither
  let (newHistory, oldHistory) = splitHistory optHistoryLength history
  putStrLn $ "Saving history file " ++ historyPath
  saveHistory historyPath newHistory
  forM_ (historyItems oldHistory) $ \item -> do
    let path = optOutputDir </> T.unpack (unHistoryItem item)
    putStrLn $ "Dropping old report " ++ path
    removeFile path

----------------------------------------------------------------------------
-- Helpers

-- | Given 'Left', print it to stdout and then exit with non-zero status
-- code.

removeEither :: Either String b -> IO b
removeEither (Left err) = do
  putStrLn err
  exitFailure
removeEither (Right x) = return x
