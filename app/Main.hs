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

import           Lib
import qualified Lib.Config                    as Config
import           Lib.FSUtils                    ( createImagesDir )
import qualified Lib.Logger                    as Logger
import qualified Lib.Logger.Impl.FileHandle    as Logger.Impl.FileHandle
import qualified Lib.Logic                     as Logic
import qualified Lib.Logic.Impl.PostgreSQL     as Logic.Impl.PostgreSQL
import qualified Lib.Routes                    as Routes

main :: IO ()
main = do
  conf  <- Config.read configFile
  fileH <- openFile (logFile conf) AppendMode
  let logger =
        Logger.logg $ Logger.Impl.FileHandle.newHandle fileH (logLevel conf)
  let postgresH = Logic.Impl.PostgreSQL.newHandle (connection conf) logger
  createImagesDir logger
  putStrLn "Server started"
  l <- mkReqLogger fileH conf
  run 8080 $ l $ Routes.runApp postgresH

mkReqLogger :: Handle -> Config -> IO Middleware
mkReqLogger hnd conf = RL.mkRequestLogger $ def
  { RL.outputFormat = case logLevel conf of
    Logger.LogDebug  -> RL.Detailed False
    Logger.LogNormal -> RL.Apache RL.FromSocket
    Logger.LogQuiet  -> RL.CustomOutputFormat (\_ _ _ _ -> "")
  , RL.autoFlush    = True
  , RL.destination  = RL.Handle hnd
  }
