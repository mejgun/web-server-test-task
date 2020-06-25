module Lib.Logger.Impl.FileHandle
  ( newHandle
  )
where

import           System.IO                      ( hFlush
                                                , hPutStrLn
                                                , Handle
                                                )

import qualified Lib.Logger                    as Logger

newHandle :: System.IO.Handle -> Logger.LogLevel -> Logger.Handle
newHandle fileH appLogLvl = Logger.Handle
  { Logger.logg = \msgLogLvl msg -> if appLogLvl <= msgLogLvl
                    then hPutStrLn fileH msg >> hFlush fileH
                    else return ()
  }
