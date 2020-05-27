#!/bin/bash
set -x 

./curl.sh http://127.0.0.1:8080/deleteuser --data '{"login":"login2","token":"036779522d916996be6944f885ce1af5"}' 
./curl.sh http://127.0.0.1:8080/deleteuser --data '{"login":"login2","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh http://127.0.0.1:8080/deleteuser --data '{"login":"nologin","token":"97b0febcad13268a5a12de9d09436ab5"}'
