module Lib.FSUtils where

import           Control.Exception
import qualified Data.ByteString               as B
                                                ( ByteString
                                                , writeFile
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                , getPermissions
                                                , readable
                                                , removeFile
                                                , writable
                                                )

import qualified Lib.Constants                 as Constants
import qualified Lib.Logger                    as Logger
import qualified Lib.Types                     as Types

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

deleteFile :: Logger.Logger -> FilePath -> IO ()
deleteFile l f =
  removeFile f
    `catch` (\e ->
              l Logger.LogQuiet
                ("Cannot delete file. " ++ (show (e :: IOException)))
                >> throw Types.ErrorInternal
            )

saveFile :: Logger.Logger -> FilePath -> B.ByteString -> IO ()
saveFile l f dat =
  B.writeFile f dat
    `catch` (\e ->
              l Logger.LogQuiet
                ("Cannot save file. " ++ (show (e :: IOException)))
                >> throw Types.ErrorInternal
            )
