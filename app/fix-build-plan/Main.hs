{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Map (Map)
import Data.Text (Text)
import Distribution.Types.PackageName
import Options.Applicative
import Stackage.BuildPlan
import qualified Data.Map.Strict as M
import qualified Data.Yaml       as Yaml

-- | Command line options.

data Options = Options
  { optBuildPlanPath :: FilePath
    -- ^ Path to the YAML build file to update
  , optSourceUrlsPath :: FilePath
    -- ^ Path to the YAML file specifying source urls to use
  }

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser) . mconcat $
  [ fullDesc
  , progDesc "Update a build plan setting source urls"
  , header "fix-build-plan"
  ]

optionsParser :: Parser Options
optionsParser = Options
  <$> argument str (metavar "BUILD-PLAN")
  <*> argument str (metavar "SOURCE-URL-FILE")

-- | Load a build plan from a YAML file and set source urls for certain
-- packages according to the data from another YAML file. The second file
-- should contain just a top-level dictionary from package names to their
-- respective URLs.

main :: IO ()
main = do
  Options {..} <- execParser optionsParserInfo
  sourceUrls <- Yaml.decodeFileThrow optSourceUrlsPath
  buildPlan <- Yaml.decodeFileThrow optBuildPlanPath
  Yaml.encodeFile optBuildPlanPath (setSourceUrls buildPlan sourceUrls)

-- | Update a 'BuildPlan' setting source urls according to the second
-- element.

setSourceUrls
  :: BuildPlan         -- ^ Build plan to update
  -> Map String Text   -- ^ Map from package names to URLs
  -> BuildPlan         -- ^ Updated build plan
setSourceUrls buildPlan sourceUrls = buildPlan
  { bpPackages = M.mapWithKey f (bpPackages buildPlan)
  }
  where
    f pname p =
      case M.lookup (unPackageName pname) sourceUrls of
        Nothing -> p
        Just url -> p { ppSourceUrl = Just url }
