module Lib.FSUtils where

import           Control.Exception
import           System.Directory               ( createDirectoryIfMissing
                                                , getPermissions
                                                , readable
                                                , writable
                                                )

import qualified Lib.Constants                 as Constants
import qualified Lib.Logger                    as Logger

createImagesDir :: Logger.Logger -> IO ()
createImagesDir l = do
  handle (\e -> l Logger.LogQuiet (show (e :: IOException)) >> throw e)
    $ createDirectoryIfMissing False Constants.imagesDir
  p <- getPermissions Constants.imagesDir
  if readable p && writable p
    then return ()
    else do
      let e = Constants.imagesDir ++ " access denied"
      l Logger.LogQuiet e
      error e
