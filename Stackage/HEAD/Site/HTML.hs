{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stackage.HEAD.Site.HTML
  ( overviewP
  , BuildPageArgs (..)
  , buildP
  , DiffPageArgs (..)
  , diffP
  , packageP
  , buildLogP
  , testLogP )
where

import Control.Monad
import Control.Monad.Trans
import Data.Monoid ((<>))
import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttribute)
import Stackage.HEAD.BuildDiff
import Stackage.HEAD.BuildResults
import Stackage.HEAD.History
import Stackage.HEAD.Package
import Stackage.HEAD.Site.Location
import Stackage.HEAD.Site.Resource
import Stackage.HEAD.Site.Type
import Stackage.HEAD.Trac
import Text.URI (URI)
import qualified Data.HashMap.Strict as HM
import qualified Text.URI            as URI

-- | Render the overview page.

overviewP :: [HistoryItem] -> DiffTable -> HtmlT IO ()
overviewP items diffTable = withDefault "Overview" $ do
  breadcrumb [("Overview", overviewUrl)]
  table_ [class_ "table table-sm"] $ do
    thead_ . tr_ $ do
      th_ [scope_ "col"] "Build (newer first)"
      th_ [scope_ "col"] "Diff"
    forM_ items $ \item -> do
      buildUrl <- reifyLocation (buildL item)
      case HM.lookup item diffTable of
        Nothing -> tr_ $ do
          td_ $ a_ [href_ buildUrl] (toHtml $ hip item)
          td_ "no info"
        Just (olderItem, (innocent, suspicious)) -> do
          let hasSuspicious = not (isEmptyDiff suspicious)
              hasInnocent   = not (isEmptyDiff innocent)
              classes
                | hasSuspicious = [class_ "table-danger"]
                | hasInnocent   = [class_ "table-success"]
                | otherwise     = []
              diffText
                | hasSuspicious = "suspicious changes"
                | hasInnocent   = "innocent changes"
                | otherwise     = "no changes"
          diffUrl <- reifyLocation (diffL olderItem item)
          tr_ classes $ do
            td_ $ a_ [href_ buildUrl] (toHtml $ hip item)
            td_ $ a_ [href_ diffUrl]  diffText
  p_ [class_ "text-muted"] $ do
    toHtml $
      "This static site has been generated automatically, see " <>
      ("the repo of the Stackage HEAD project here: " :: Text)
    a_ [href_ githubRepoUrl] (toHtml githubRepoUrl)

data BuildPageArgs = BuildPageArgs
  { bpaItem :: HistoryItem
  , bpaBuildResults :: BuildResults
  , bpaDiffTable :: DiffTable
  , bpaBuildUrl :: URI
  }

-- | Render results of a build.

buildP :: BuildPageArgs -> HtmlT IO ()
buildP BuildPageArgs {..} = withDefault "Build results" $ do
  buildUrl   <- reifyLocation (buildL bpaItem)
  breadcrumb [ ("Overview", overviewUrl)
             , ("Build " <> hip bpaItem, buildUrl)
             ]
  let (stackagePlan, ghcCommit) = decomposeHistoryItem bpaItem
      cabalConfigUrl' = URI.render (cabalConfigUrl stackagePlan)
      ghcCommitUrl'   = URI.render (ghcCommitUrl ghcCommit)
  btnLink cabalConfigUrl' "Cabal config"
  btnLink ghcCommitUrl' "GHC commit"
  btnLink (URI.render bpaBuildUrl) "CircleCI build"
  forM_ (HM.lookup bpaItem bpaDiffTable) $ \(olderItem, _) -> do
    diffUrl <- reifyLocation (diffL olderItem bpaItem)
    btnLink diffUrl "Diff with prev build"
  table_ [class_ "table table-sm"] $ do
    thead_ . tr_ $ do
      th_ [scope_ "col"] "Package"
      th_ [scope_ "col"] "Build"
      th_ [scope_ "col"] "Test"
    forM_ (buildResultsItems bpaBuildResults) $ \(packageName, status) -> do
      packageUrl <- reifyLocation (packageL bpaItem packageName)
      buildLogUrl <- reifyLocation (buildLogL bpaItem packageName)
      testLogUrl <- reifyLocation (testLogL bpaItem packageName)
      let (buildSummary, testSummary, classes) =
            case status of
              BuildFailure n   ->
                ( a_ [href_ buildLogUrl] ("failing, blocking " <> toHtml (show n))
                , "no info"
                , [class_ "table-danger"]
                )
              BuildUnreachable ->
                ( a_ [href_ buildLogUrl] "unreachable"
                , "no info"
                , [class_ "table-secondary"]
                )
              BuildSuccess passing failing ->
                ( a_ [href_ buildLogUrl] "success"
                , a_ [href_ testLogUrl] $ do
                    toHtml (show passing)
                    span_ [class_ "fa fa-check"] (return ())
                    toHtmlRaw ("&nbsp;" :: Text)
                    toHtml (show failing)
                    span_ [class_ "fa fa-times"] (return ())
                , []
                )
      tr_ classes $ do
        td_ $ a_ [href_ packageUrl] (toHtml $ unPackageName packageName)
        td_ buildSummary
        td_ testSummary

data DiffPageArgs = DiffPageArgs
  { dpaOlderItem :: HistoryItem
  , dpaNewerItem :: HistoryItem
  , dpaInnocentDiff :: BuildDiff
  , dpaSuspiciousDiff :: BuildDiff
  , dpaBuildUrl  :: URI
  }

-- | Render a diff between two builds.

diffP
  :: DiffPageArgs
  -> HtmlT IO ()
diffP DiffPageArgs {..} = withDefault "Diff" $ do
  diffUrl <- reifyLocation (diffL dpaOlderItem dpaNewerItem)
  olderBuildUrl <- reifyLocation (buildL dpaOlderItem)
  newerBuildUrl <- reifyLocation (buildL dpaNewerItem)
  breadcrumb [ ("Overview", overviewUrl)
             , ("Diff " <> hip dpaNewerItem, diffUrl)
             ]
  let tracTicket = generateTracTicket
        dpaBuildUrl
        (dpaOlderItem, dpaNewerItem)
        dpaSuspiciousDiff
  unless (isEmptyDiff dpaSuspiciousDiff) $
    form_ $
      div_ [class_ "form-group"] $ do
        label_ [for_ "trac-ticket-content"] "Generated Trac ticket"
        textarea_ [ id_ "trac-ticket-content"
                  , type_ "hidden"
                  , class_ "form-control"
                  , rows_ "10"
                  ] $
          toHtml tracTicket
  btnLink olderBuildUrl "Older build"
  btnLink newerBuildUrl "Newer build"
  when (isEmptyDiff dpaSuspiciousDiff && isEmptyDiff dpaInnocentDiff) $
    p_ "No changes."
  unless (isEmptyDiff dpaSuspiciousDiff) $ do
    button_ [ makeAttribute "data-clipboard-target" "#trac-ticket-content"
            , class_ "btn btn-light mb-3 mr-3"
            , id_ "trac-button"
            ]
      "Copy Trac ticket"
    h3_ "Suspicious changes"
    forM_ (buildDiffItems dpaSuspiciousDiff) $
      renderPackageDiff (dpaOlderItem, olderBuildUrl)
                        (dpaNewerItem, newerBuildUrl)
  unless (isEmptyDiff dpaInnocentDiff) $ do
    h3_ "Innocent changes"
    forM_ (buildDiffItems dpaInnocentDiff) $
      renderPackageDiff (dpaOlderItem, olderBuildUrl)
                        (dpaNewerItem, newerBuildUrl)

-- | Render diff for a single package.

renderPackageDiff
  :: (HistoryItem, Text) -- ^ Older history item and its url
  -> (HistoryItem, Text) -- ^ Newer history item and its url
  -> (PackageName, ( Maybe BuildStatus
                   , Maybe BuildStatus ))
  -> HtmlT IO ()
renderPackageDiff (oitem, ourl) (nitem, nurl) (packageName, (ostate, nstate)) =
  div_ [class_ "card my-3"] $ do
    div_ [class_ "card-body"] $ do
      h5_ [class_ "card-title"] (toHtml (unPackageName packageName))
      ul_ $ do
        li_ $ do
          "at "
          a_ [href_ ourl] (toHtml $ hip oitem)
          " (older)"
          ul_ $ li_ (renderPackageState oitem packageName ostate)
        li_ $ do
          "at "
          a_ [href_ nurl] (toHtml $ hip nitem)
          " (newer)"
          ul_ $ li_ (renderPackageState nitem packageName nstate)

-- | Render a single package state.

renderPackageState
  :: HistoryItem
  -> PackageName
  -> Maybe BuildStatus
  -> HtmlT IO ()
renderPackageState item packageName mstatus =
  case mstatus of
    Nothing -> "not present"
    Just x -> do
      buildLogUrl <- reifyLocation (buildLogL item packageName)
      testLogUrl <- reifyLocation (testLogL item packageName)
      case x of
        BuildFailure _ ->
          a_ [href_ buildLogUrl] "build failure"
        BuildUnreachable ->
          a_ [href_ buildLogUrl] "build unreachable"
        BuildSuccess p b -> do
          a_ [href_ buildLogUrl] "build succeeded"
          ", "
          a_ [href_ testLogUrl] $ toHtml $
            show p <> " test suites passed, "
            <> show b <> " test suites failed"

-- | Render information about a package in a build.

packageP
  :: HistoryItem
  -> PackageName
  -> BuildStatus
  -> HtmlT IO ()
packageP item packageName status = withDefault "Package" $ do
  buildUrl <- reifyLocation (buildL item)
  packageUrl <- reifyLocation (packageL item packageName)
  buildLogUrl <- reifyLocation (buildLogL item packageName)
  testLogUrl <- reifyLocation (testLogL item packageName)
  breadcrumb [ ("Overview", overviewUrl)
             , ("Build " <> hip item, buildUrl)
             , (unPackageName packageName, packageUrl)
             ]
  let buildLogBtn = btnLink buildLogUrl "Build log"
      testLogBtn = btnLink testLogUrl "Test log"
  case status of
    BuildFailure n -> do
      buildLogBtn
      p_ . toHtml $ "The build failed. It prevents " <>
        show n <> " other packages from building."
    BuildUnreachable ->
      p_ $ "The build is unreachable."
    BuildSuccess p n -> do
      buildLogBtn
      testLogBtn
      p_ . toHtml $ "The build succeeded. " <> show p <>
        " test suites succeeded, " <> show n <> " failed."

-- | Render a build log.

buildLogP
  :: HistoryItem
  -> PackageName
  -> Text
  -> HtmlT IO ()
buildLogP item packageName l = withDefault "Build log" $ do
  buildUrl   <- reifyLocation (buildL item)
  packageUrl <- reifyLocation (packageL item packageName)
  logUrl     <- reifyLocation (buildLogL item packageName)
  breadcrumb [ ("Overview", overviewUrl)
             , ("Build " <> hip item, buildUrl)
             , (unPackageName packageName, packageUrl)
             , ("Build log", logUrl)
             ]
  pre_ $ code_ $ toHtml l

-- | Reder a test log.

testLogP
  :: HistoryItem
  -> PackageName
  -> Text
  -> HtmlT IO ()
testLogP item packageName l = withDefault "Test log" $ do
  buildUrl   <- reifyLocation (buildL item)
  packageUrl <- reifyLocation (packageL item packageName)
  logUrl     <- reifyLocation (testLogL item packageName)
  breadcrumb [ ("Overview", overviewUrl)
             , ("Build " <> hip item, buildUrl)
             , (unPackageName packageName, packageUrl)
             , ("Test log", logUrl)
             ]
  pre_ $ code_ $ toHtml l

----------------------------------------------------------------------------
-- Helpers

withDefault :: forall m. Monad m
  => Text              -- ^ Title
  -> HtmlT m ()        -- ^ Original rendering function
  -> HtmlT m ()        -- ^ Updated rendering function
withDefault title inner = do
  let siteTitle   = "Stackage HEAD" :: Text
  doctype_
  html_ [lang_ "en", dir_ "ltr"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "x-ua-compatible", content_ "ie=edge"]
      meta_ [name_ "viewpoint", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      title_ (toHtml title)
      stylesheetResource bootstrap4Stylesheet
      stylesheetResource fontawesomeStylesheet
    body_ $ do
      nav_ [class_ "navbar navbar-expand-md navbar-dark bg-primary"] $
        div_ [class_ "container"] $
          a_ [class_ "navbar-brand", href_ overviewUrl, title_ siteTitle] $
            toHtml siteTitle
      main_ $ do
        div_ [class_ "py-5"] $
          div_ [class_ "container"] inner
      scriptResource jqueryScript
      scriptResource popperScript
      scriptResource bootstrap4Script
      scriptResource clipboardJsScript
      script_ "new ClipboardJS('#trac-button')"

breadcrumb :: forall m. Monad m => [(Text, Text)] -> HtmlT m ()
breadcrumb [] = return ()
breadcrumb xs' = do
  nav_ [makeAttribute "aria-label" "breadcrumb"] $
    ol_ [class_ "breadcrumb"] $ do
      let x  = last xs'
          xs = init xs'
          renderOne :: Bool -> (Text, Text) -> HtmlT m ()
          renderOne isLast (name, url) = do
            let classes = "breadcrumb-item" <>
                  if isLast
                    then " active"
                    else ""
                ariaCurrent = [makeAttribute "aria-current" "page"]
            li_ ([class_ classes] <> ariaCurrent) $
              a_ [href_ url] (toHtml name)
      forM_ xs (renderOne False)
      renderOne True x

btnLink :: Monad m
  => Text              -- ^ URL
  -> Text              -- ^ Title
  -> HtmlT m ()
btnLink url title =
  a_ [href_ url, class_ "btn btn-light mb-3 mr-3"] (toHtml title)

reifyLocation :: IO Location -> HtmlT IO Text
reifyLocation l = URI.render . locationUri <$> lift l

githubRepoUrl :: Text
githubRepoUrl = "https://github.com/tweag/stackage-head"
