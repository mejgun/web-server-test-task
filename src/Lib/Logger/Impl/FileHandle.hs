module Lib.Logger.Impl.FileHandle
  ( newHandle
  )
where

import           System.IO                      ( Handle
                                                , hFlush
                                                , hPutStrLn
                                                )

import qualified Lib.Logger                    as Logger

newHandle :: System.IO.Handle -> Logger.LogLevel -> Logger.Handle
newHandle fileH appLogLvl = Logger.Handle
  { Logger.logg = \msgLogLvl msg -> if appLogLvl <= msgLogLvl
                    then hPutStrLn fileH msg >> hFlush fileH
                    else return ()
  }
