module Lib.Logger.Impl.Null
  ( newHandle
  )
where

import qualified Lib.Logger                    as Logger

newHandle :: h -> Logger.LogLevel -> Logger.Handle
newHandle _ _ = Logger.Handle { Logger.logg = \_ _ -> return () }
