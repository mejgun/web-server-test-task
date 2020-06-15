#!/bin/bash
set -x 

: delete category
./curl.sh category/delete '{"cat_id":3,"token":"97b0febcad13268a5a12de9d09436ab5"}'

: delete category - category not exist
./curl.sh category/delete '{"cat_id":999999,"token":"97b0febcad13268a5a12de9d09436ab5"}'

: delete category - user not admin
./curl.sh category/delete '{"cat_id":2,"token":"8e37ca708c492c66383247ae57009531"}'
