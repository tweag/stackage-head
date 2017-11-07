#!/usr/bin/env runhaskell

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit

-- cheap'n'cheesy build-constraints.yaml pruning...we really should properly parse the yaml etc

main :: IO ()
main
  = do
    { args <- getArgs
    ; unless (length args == 2) $ do
        { putStrLn "usage: PruneConstraints.hs BUILD-CONSTRAINTS INVALID-PACKAGES"
        ; exitFailure
        }
    ; let [buildConstraintsFName, packagesFName] = args
    ; buildConstraints <- readFile buildConstraintsFName
    ; invalidPackages  <- readFile packagesFName
    ; putStr $ prunedBuildConstraints buildConstraints (words invalidPackages)
    }

prunedBuildConstraints :: String -> [String] -> String
prunedBuildConstraints buildConstraints invalidPackages
  = unlines $ prologue ++ ("packages:" : prune (filter (not . null) packages) invalidPackages) ++ epilogue
  where
    buildConstraintsLines = removeEmptyLines . lines $ buildConstraints
    (prologue, rest)      = break (== "packages:") buildConstraintsLines
    (packages, epilogue)  = break (\line -> length line > 0 && (not . isSpace $ head line)) (tail rest)
    
data Stanza  = Stanza String [Package]    -- maintainer & maintainer's package list
data Package = Package String String      -- package name & original line representing that package in the source 

prune :: [String] -> [String] -> [String]
prune packages invalidPackages
  = concat (unStanza (pruneStanzas invalidPackages (stanzas indentGroups)))
  where
    indentGroups = groupBy sameIndents packages
    
    stanzas :: [[String]] -> [Stanza]
    stanzas []                      = []
    stanzas ([header]:body:rest)    = Stanza header (map package body) : stanzas rest
    stanzas ((header:headers):rest) = Stanza header []                 : stanzas (headers:rest)
    
    package line = Package (packageName line) line
    
    packageName line = 
      case dropWhile isSpace line of
        '-':rest -> head $ words rest ++ [""]
        _        -> ""
    
    unStanza []                             = []
    unStanza (Stanza header body : stanzas) = (header : unPackages body) : unStanza stanzas
    
    unPackages = map (\(Package _ line) -> line)

pruneStanzas :: [String] -> [Stanza] -> [Stanza]
pruneStanzas invalidPackages
  = catMaybes . map pruneStanza
  where
    pruneStanza (Stanza header [Package "" "[]"]) = Nothing
    pruneStanza (Stanza header packages)
      | null packages'                            = Nothing
      | otherwise                                 = Just $ Stanza header packages'
      where
        packages' = filter (\pck@(Package name _) -> not (any (name ==) invalidPackages)) packages
   
sameIndents :: String -> String -> Bool   
sameIndents line1 line2 = length (takeWhile isSpace line1) == length (takeWhile isSpace line2)

-- Remove all lines that contain only whitespaces and possibly a comment.
--
removeEmptyLines :: [String] -> [String]
removeEmptyLines = catMaybes . map removeEmptyLine
  where
    removeEmptyLine line | null nonWS || head nonWS == '#' = Nothing
                         | otherwise                       = Just line
      where
        nonWS = dropWhile isSpace line
      