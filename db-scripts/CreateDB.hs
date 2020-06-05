{-# LANGUAGE OverloadedStrings #-}

import           PG

import           Database.PostgreSQL.Simple.Types
                                                ( Query(..) )
import qualified Data.ByteString               as B
                                                ( readFile )

main :: IO ()
main = do
  conn <- pgconnect
  print =<< id =<< execute_ conn <$> Query <$> B.readFile "db-scripts/db.sql"
