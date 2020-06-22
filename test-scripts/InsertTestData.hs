{-# LANGUAGE OverloadedStrings #-}

import           Lib

main :: IO ()
main = do
  c <- readConfig configFile
  mapM_
    (\x -> print =<< execute_ (connection c) x)
    [ "insert into users (name,lastname,admin,token,login,password) values ('testadmin','testlastname',true,'97b0febcad13268a5a12de9d09436ab5','admin',md5('adminpass'));"
    , "insert into users (name,lastname,token,login,password) values ('testuser','testlastname','036779522d916996be6944f885ce1af5','user',md5('userpass'));"
    , "insert into users (name,lastname,token,login,password) values ('testauthoruser','testauthorlastname','8e37ca708c492c66383247ae57009531','author1',md5('authorpass'));"
    , "insert into users (name,lastname,token,login,password) values ('testauthoruser2','testauthorlastname2','b185b4ee226a682ff359b7640212c1c5','author2',md5('authorpass'));"
    ]
