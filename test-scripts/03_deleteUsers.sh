#!/bin/bash
set -x 

: delete user - not admin
./curl.sh user/delete '{"login":"login3","token":"036779522d916996be6944f885ce1af5"}' 

: delete user - bad json
./curl.sh user/delete '{"ogin":"login3","token":"036779522d916996be6944f885ce1af5"}' 

: delete user 
./curl.sh user/delete '{"login":"todelete","token":"97b0febcad13268a5a12de9d09436ab5"}'

: delete user - user not exist
./curl.sh user/delete '{"login":"nologin","token":"97b0febcad13268a5a12de9d09436ab5"}'
