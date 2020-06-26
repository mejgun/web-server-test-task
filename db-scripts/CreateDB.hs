{-# LANGUAGE OverloadedStrings #-}

import           Lib

import qualified Data.ByteString               as B
                                                ( readFile )
import           Database.PostgreSQL.Simple.Types
                                                ( Query(..) )

main :: IO ()
main = do
  c <- readConfig configFile
  q <- Query <$> B.readFile "db-scripts/db.sql"
  r <- execute_ (connection c) q
  print r
