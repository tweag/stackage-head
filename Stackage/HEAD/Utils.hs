module Stackage.HEAD.Utils
  ( forgivingAbsence
  , ignoringAbsence )
where

import Control.Exception
import System.IO.Error (isDoesNotExistError)

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

-- | The same as 'forgivingAbsence', but ignores result.

ignoringAbsence :: IO a -> IO ()
ignoringAbsence = fmap (const ()) . forgivingAbsence
