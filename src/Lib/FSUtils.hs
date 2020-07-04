module Lib.FSUtils where

import           Control.Exception
import           System.Directory               ( createDirectoryIfMissing
                                                , getPermissions
                                                , readable
                                                , writable
                                                )

import qualified Lib.Constants                 as Constants
                                                ( imagesDir )
import qualified Lib.Logger                    as Logger

createImagesDir :: Logger.Logger -> IO ()
createImagesDir logg = do
  handle (\e -> logg Logger.LogQuiet (show (e :: IOException)) >> throw e)
    $ createDirectoryIfMissing False Constants.imagesDir
  perm <- getPermissions Constants.imagesDir
  if readable perm && writable perm
    then return ()
    else do
      let e = Constants.imagesDir ++ " access denied"
      logg Logger.LogQuiet e
      error e
