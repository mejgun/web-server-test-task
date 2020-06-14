#!/bin/bash
set -x 

: get authors
./curl.sh author/get  '{"token":"97b0febcad13268a5a12de9d09436ab5","page":1}'

: get authors - bad page
./curl.sh author/get  '{"token":"97b0febcad13268a5a12de9d09436ab5","page":0}'

: get authors - user not admin
./curl.sh author/get  '{"token":"8e37ca708c492c66383247ae57009531","page":1}'
