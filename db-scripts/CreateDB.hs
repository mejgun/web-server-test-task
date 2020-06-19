{-# LANGUAGE OverloadedStrings #-}

import           Lib

import           Database.PostgreSQL.Simple.Types
                                                ( Query(..) )
import qualified Data.ByteString               as B
                                                ( readFile )

main :: IO ()
main = do
  c <- readConfig
  q <- Query <$> B.readFile "db-scripts/db.sql"
  r <- execute_ (connection c) q
  print r
