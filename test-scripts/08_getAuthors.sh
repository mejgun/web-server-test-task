#!/bin/bash
set -x 

./curl.sh http://127.0.0.1:8080/getauthors  --data '{"token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh http://127.0.0.1:8080/getauthors/1 --data '{"token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh http://127.0.0.1:8080/getauthors/d --data '{"token":"97b0febcad13268a5a12de9d09436ab5"}'
