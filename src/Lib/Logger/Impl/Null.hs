module Lib.Logger.Impl.Null
  ( newHandle
  )
where

import qualified Lib.Logger                    as Logger

newHandle :: Logger.Handle
newHandle = Logger.Handle { Logger.logg = \_ _ -> return () }
