{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Data.Default
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import qualified Network.Wai.Middleware.RequestLogger
                                               as RL
import           System.IO                      ( Handle
                                                , IOMode(..)
                                                , openFile
                                                )

import qualified Lib.Config                    as Config
import qualified Lib.Constants                 as Constants
import qualified Lib.DB                        as DB
import qualified Lib.DB.Impl.PostgreSQL        as DB.Impl.PostgreSQL
import           Lib.FSUtils                    ( createImagesDir )
import qualified Lib.Logger                    as Logger
import qualified Lib.Logger.Impl.FileHandle    as Logger.Impl.FileHandle
import qualified Lib.Routes                    as Routes

main :: IO ()
main = do
  conf  <- Config.read Constants.configFile
  fileH <- openFile (Config.logFile conf) AppendMode
  let logger = Logger.logg
        $ Logger.Impl.FileHandle.newHandle fileH (Config.logLevel conf)
  let postgresH = DB.Impl.PostgreSQL.newHandle (Config.connection conf) logger
  createImagesDir logger
  putStrLn "Server started"
  l <- mkReqLogger fileH conf
  run 8080 $ l $ Routes.runApp postgresH logger

mkReqLogger :: Handle -> Config.Config -> IO Middleware
mkReqLogger hnd conf = RL.mkRequestLogger $ def
  { RL.outputFormat = case Config.logLevel conf of
    Logger.LogDebug  -> RL.Detailed False
    Logger.LogNormal -> RL.Apache RL.FromSocket
    Logger.LogQuiet  -> RL.CustomOutputFormat (\_ _ _ _ -> "")
  , RL.autoFlush    = True
  , RL.destination  = RL.Handle hnd
  }
