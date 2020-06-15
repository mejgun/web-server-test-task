#!/bin/bash
set -x 

: edit tag
./curl.sh tag/edit '{"tag_id":2,"name":"renamed_tag","token":"97b0febcad13268a5a12de9d09436ab5"}'

: edit tag - tag not exist
./curl.sh tag/edit '{"tag_id":999999,"name":"renamed_tag","token":"97b0febcad13268a5a12de9d09436ab5"}'

: edit tag - user not admin
./curl.sh tag/edit '{"tag_id":2,"name":"renamed_tag","token":"8e37ca708c492c66383247ae57009531"}'
