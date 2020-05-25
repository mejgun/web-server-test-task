{-# LANGUAGE OverloadedStrings #-}

import           PG

main :: IO ()
main = do
  conn <- pgconnect
  mapM_
    (\x -> print =<< execute_ conn x)
    [ "insert into users (name,lastname,admin,token,login,password) values ('testadmin','testlastname',true,'97b0febcad13268a5a12de9d09436ab5','admin',md5('adminpass'));"
    , "insert into users (name,lastname,token,login,password) values ('testuser','testlastname','036779522d916996be6944f885ce1af5','user',md5('userpass'));"
    ]
