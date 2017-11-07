#!/usr/bin/env runhaskell

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit

-- add give invalid packages to skipped tests and bechmarks.

main :: IO ()
main
  = do
    { args <- getArgs
    ; unless (length args == 2) $ do
        { putStrLn "usage: SkipTestBenchs.hs BUILD-CONSTRAINTS INVALID-PACKAGES"
        ; exitFailure
        }
    ; let [buildConstraintsFName, packagesFName] = args
    ; buildConstraints <- readFile buildConstraintsFName
    ; invalidPackages  <- readFile packagesFName
    ; putStr $ addSkips buildConstraints (words invalidPackages)
    }

addSkips :: String -> [String] -> String
addSkips buildConstraints invalidPackages
  = unlines $ 
      prologue 
      ++ ("skipped-tests:" : indentedInvalidPackages) 
      ++ middle 
      ++ ("skipped-benchmarks:" : indentedInvalidPackages) 
      ++ tail epilogue
  where
    buildConstraintsLines   = removeEmptyLines . lines $ buildConstraints
    (prologue, rest)        = break (== "skipped-tests:") buildConstraintsLines
    (middle, epilogue)      = break (== "skipped-benchmarks:") (tail rest)
    indentedInvalidPackages = map ("    - " ++) invalidPackages

-- Remove all lines that contain only whitespaces and possibly a comment.
--
removeEmptyLines :: [String] -> [String]
removeEmptyLines = catMaybes . map removeEmptyLine
  where
    removeEmptyLine line | null nonWS || head nonWS == '#' = Nothing
                         | otherwise                       = Just line
      where
        nonWS = dropWhile isSpace line
