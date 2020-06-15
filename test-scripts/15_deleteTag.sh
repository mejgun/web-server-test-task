#!/bin/bash
set -x 

: delete tag
./curl.sh tag/delete '{"tag_id":3,"token":"97b0febcad13268a5a12de9d09436ab5"}'

: delete tag - tag not exist
./curl.sh tag/delete '{"tag_id":999999,"token":"97b0febcad13268a5a12de9d09436ab5"}'

: delete tag - user not admin
./curl.sh tag/delete '{"tag_id":3,"token":"8e37ca708c492c66383247ae57009531"}'
