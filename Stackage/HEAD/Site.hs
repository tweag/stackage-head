{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Stackage.HEAD.Site
  ( SiteParams (..)
  , generateSite )
where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lucid
import Path
import Path.IO
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.BuildLogs
import Stackage.HEAD.BuildResults
import Stackage.HEAD.History
import Stackage.HEAD.Site.HTML
import Stackage.HEAD.Site.Location
import Stackage.HEAD.Site.Type
import Stackage.HEAD.Utils
import Text.URI (URI)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy.IO    as TL
import qualified Text.URI             as URI
import qualified Text.URI.QQ          as Q

-- | Generate static site for a single diff.

generateSite :: SiteParams -> IO ()
generateSite SiteParams {..} = do
  let savePageAs loc content = do
        let opath = spLocation </> locationPath loc
        ensureDir (parent opath)
        renderTextT content >>= TL.writeFile (fromAbsFile opath)
  history <- loadHistory (fromAbsFile spHistoryFile) >>= removeEither
  let items = historyItems history
  pairs <- forM items $ \item -> do
    buildReportPath <- resolveFile spBuildReports (strHistoryItem item)
    rawBytes <- BL.readFile (fromAbsFile buildReportPath)
    (item, ) <$> removeEither (decodeBuildResults rawBytes)
  let diffTable = HM.fromList $ zipWith
        (\(oldItem, oldResults) (newItem, newResults) ->
            ( newItem
            , ( oldItem
              , partitionByInnocence
                (diffBuildResults oldResults newResults)
              )))
        (drop 1 pairs)
        pairs
  savePageAs overviewL (overviewP items diffTable)
  forM_ pairs $ \(item, br) -> do
    forM_ (HM.toList $ unBuildResults br) $ \(packageName, status) -> do
      let p = fromAbsDir spBuildReports
      buildLog <- retrieveBuildLog p item packageName
      testLog <- retrieveTestLog p item packageName
      l <- packageL item packageName
      savePageAs l $ packageP PackagePageArgs
        { ppaItem = item
        , ppaPackageName = packageName
        , ppaBuildStatus = status
        , ppaBuildLog = buildLog
        , ppaTestLog = testLog
        }
    bl <- buildL item
    buildUrl <- resolveFile spBuildReports (historyItemToBuildUrl item)
      >>= fmap fixupUrl . forgivingAbsence . T.readFile . fromAbsFile
    savePageAs bl $ buildP BuildPageArgs
      { bpaItem = item
      , bpaBuildResults = br
      , bpaDiffTable = diffTable
      , bpaBuildUrl = buildUrl
      }
    forM_ (HM.lookup item diffTable) $ \r@(oitem, (idiff, sdiff)) -> do
      dl <- diffL (fst r) item
      savePageAs dl $ diffP DiffPageArgs
        { dpaOlderItem = oitem
        , dpaNewerItem = item
        , dpaInnocentDiff = idiff
        , dpaSuspiciousDiff = sdiff
        , dpaBuildUrl = buildUrl
        }

fixupUrl :: Maybe Text -> URI
fixupUrl raw = fromMaybe [Q.uri|https://circleci.com|] $
  raw >>= URI.mkURI . T.strip
