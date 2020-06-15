{-# LANGUAGE OverloadedStrings #-}



import           Database.PostgreSQL.Simple.Types
                                                ( Query(..) )
import qualified Data.ByteString               as B
                                                ( readFile )

main :: IO ()
main = do
  c <- pgconnect
  q <- Query <$> B.readFile "db-scripts/db.sql"
  r <- execute_ c q
  print r
