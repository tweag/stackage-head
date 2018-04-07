{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Text (Text)
import Data.Vector (Vector)
import Options.Applicative
import Stackage.HEAD.Parser
import Stackage.HEAD.Type
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.IO (withFile, hPutStrLn, IOMode (..))
import System.IO.Error (isDoesNotExistError)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.IO         as TIO
import qualified Data.Vector          as V
import qualified Text.Megaparsec      as M

main :: IO ()
main = do
  Options {..} <- execParser optionsParserInfo
  emetadata <- Aeson.eitherDecodeStrict' <$> B.readFile optMetadata
  case emetadata of
    Left err -> do
      putStrLn err
      exitFailure
    Right BuildInfo {..} -> do
      createDirectoryIfMissing True optOutputDir
      let reportPath = optOutputDir </> reportBasename
          reportBasename = (optTarget ++ "-" ++ biSha1) <.> "csv"
          historyPath = optOutputDir </>
            "history" <.> "csv"
      history <- loadHistory historyPath
      when (presentInHistory history (T.pack reportBasename)) $ do
        putStrLn $ "Report " ++ reportBasename
          ++ " is already present in history, so do nothing."
        exitSuccess
      rawText <- TIO.readFile optBuildLog
      case parseBuildLog optBuildLog rawText of
        Left err -> do
          putStr (M.parseErrorPretty' rawText err)
          exitFailure
        Right buildResults -> do
          putStrLn $ "Saving report to " ++ reportPath
          BL.writeFile reportPath (encodeBuildResults buildResults)
          extendHistory historyPath reportBasename

----------------------------------------------------------------------------
-- History manipulation

newtype History = History
  { unHistory :: Vector Text
  }

newtype HistoryItem = HistoryItem
  { unHistoryItem :: Text
  }

instance Csv.FromRecord HistoryItem where
  parseRecord v = do
    bs <- v Csv..! 0
    (return . HistoryItem . TE.decodeUtf8) bs

loadHistory :: FilePath -> IO History
loadHistory path = do
  putStrLn $ "Checking history file " ++ path
  mhistory <- forgivingAbsence (BL.readFile path)
  case mhistory of
    Nothing    -> return (History V.empty)
    Just bytes ->
      case Csv.decode Csv.NoHeader bytes of
        Left err -> do
          putStrLn err
          exitFailure
        Right history ->
          (return . History . fmap unHistoryItem) history

presentInHistory :: History -> Text -> Bool
presentInHistory (History v) report = V.elem report v

extendHistory :: FilePath -> FilePath -> IO ()
extendHistory path reportBasename = do
  putStrLn $ "Extending history file " ++ path
  withFile path AppendMode $ \h ->
    hPutStrLn h reportBasename

----------------------------------------------------------------------------
-- Command link interface

-- | Command line options.

data Options = Options
  { optMetadata :: FilePath
    -- ^ Location of GHC build metadata file
  , optBuildLog :: FilePath
    -- ^ Location of raw Stackage curator build log
  , optTarget :: String
    -- ^ Target name, without extension
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
    ]
  <*> (strOption . mconcat)
    [ long "outdir"
    , metavar "OUTPUT-DIR"
    , help "Directory where to save resulting reports."
    ]

-- | If argument of the function throws a
-- 'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
-- exceptions propagate). Otherwise the result is returned inside a 'Just'.

forgivingAbsence :: IO a -> IO (Maybe a)
forgivingAbsence f = catchJust p
  (Just <$> f)
  (\() -> return Nothing)
  where
    p e = if isDoesNotExistError e
            then Just ()
            else Nothing
