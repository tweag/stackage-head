module Stackage.HEAD.Utils
  ( removeEither )
where

import System.Exit (exitFailure)

-- | Given 'Left', print it to stdout and then exit with non-zero status
-- code.

removeEither :: Either String b -> IO b
removeEither (Left err) = do
  putStrLn err
  exitFailure
removeEither (Right x) = return x
