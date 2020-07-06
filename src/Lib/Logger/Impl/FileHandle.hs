module Lib.Logger.Impl.FileHandle
  ( newHandle
  )
where
import           Control.Monad                  ( when )
import           System.IO                      ( Handle
                                                , hFlush
                                                , hPutStrLn
                                                )

import qualified Lib.Logger                    as Logger

newHandle :: System.IO.Handle -> Logger.LogLevel -> Logger.Handle
newHandle fileH appLogLvl = Logger.Handle
  { Logger.logg = \msgLogLvl msg -> when (appLogLvl <= msgLogLvl)
                                         (hPutStrLn fileH msg >> hFlush fileH)
  }
