{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Stackage.HEAD.Site
  ( SiteParams (..)
  , generateSite )
where

import Control.Monad
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Data.Map.Strict      as M
import qualified Data.Text.Lazy.IO    as TL

-- | Generate static site for a single diff.

generateSite :: SiteParams -> IO ()
generateSite SiteParams {..} = do
  let savePageAs loc content = do
        let opath = spLocation </> locationPath loc
        ensureDir (parent opath)
        renderTextT content >>= TL.writeFile (fromAbsFile opath)
  history <- loadHistory spHistoryFile >>= removeEither
  let items = historyItems history
  pairs <- forM items $ \item -> do
    let buildReportPath = spBuildReports </> reportPath item
    rawBytes <- BL.readFile (fromAbsFile buildReportPath)
    (item, ) <$> removeEither (decodeBuildResults rawBytes)
  let diffTable = M.fromList $ zipWith
        (\(oldItem, oldResults) (newItem, newResults) ->
            ( newItem
            , ( oldItem
              , partitionByInnocence spFlakyPkgs
                (diffBuildResults oldResults newResults)
              )))
        (drop 1 pairs)
        pairs
  savePageAs overviewL (overviewP items diffTable)
  forM_ pairs $ \(item, br) -> do
    forM_ (HM.toList $ unBuildResults br) $ \(packageName, status) -> do
      buildLog <- retrieveBuildLog spBuildReports item packageName
      testLog <- retrieveTestLog spBuildReports item packageName
      l <- packageL item packageName
      savePageAs l $ packageP PackagePageArgs
        { ppaItem = item
        , ppaPackageName = packageName
        , ppaBuildStatus = status
        , ppaBuildLog = buildLog
        , ppaTestLog = testLog
        , ppaFlaky = HS.member packageName spFlakyPkgs
        }
    bl <- buildL item
    let buildUrl = hitemBuildUrl item
    savePageAs bl $ buildP BuildPageArgs
      { bpaItem = item
      , bpaBuildResults = br
      , bpaDiffTable = diffTable
      , bpaBuildUrl = buildUrl
      , bpaFlakyPkgs = spFlakyPkgs
      }
    forM_ (M.lookup item diffTable) $ \r@(oitem, (idiff, sdiff)) -> do
      dl <- diffL (fst r) item
      savePageAs dl $ diffP DiffPageArgs
        { dpaOlderItem = oitem
        , dpaNewerItem = item
        , dpaInnocentDiff = idiff
        , dpaSuspiciousDiff = sdiff
        , dpaBuildUrl = buildUrl
        , dpaFlakyPkgs = spFlakyPkgs
        }
