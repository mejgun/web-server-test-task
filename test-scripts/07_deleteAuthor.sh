#!/bin/bash
set -x 

: delete author
./curl.sh author/delete '{"login":"asdlogin","token":"97b0febcad13268a5a12de9d09436ab5"}'

: delete author - author not exist
./curl.sh author/delete '{"login":"l","token":"97b0febcad13268a5a12de9d09436ab5"}'

: delete author - user not admin
./curl.sh author/delete '{"login":"l","token":"8e37ca708c492c66383247ae57009531"}'
