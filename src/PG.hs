module PG
  ( pgconnect
  , module Database.PostgreSQL.Simple
  )
where

import qualified Data.ByteString               as B
import           Database.PostgreSQL.Simple

pgconnect :: IO Connection
pgconnect = B.readFile "pgconfig.txt" >>= connectPostgreSQL
