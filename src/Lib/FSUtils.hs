module Lib.FSUtils where

import           Control.Exception
import           Control.Monad.Except
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
import           System.Directory               ( removeFile )

import qualified Lib.Constants                 as Constants
import qualified Lib.Logger                    as Logger
import qualified Lib.Logic                     as Logic
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

deleteFile :: Logger.Logger -> FilePath -> IO Bool
deleteFile logg file = do
  res <- try $ removeFile file
  case res of
    Left e -> do
      logg Logger.LogQuiet $ "Cannot delete file. " <> show (e :: IOException)
      throw Logic.ErrorInternal
    _ -> return True

saveFile :: Logger.Logger -> FilePath -> B.ByteString -> IO Bool
saveFile logg file dat = do
  res <- try $ B.writeFile file dat
  case res of
    Left e -> do
      logg Logger.LogQuiet $ "Cannot save file. " <> show (e :: IOException)
      throw Logic.ErrorInternal
    _ -> return True
