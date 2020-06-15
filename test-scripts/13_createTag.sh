#!/bin/bash
set -x 

: create tag
./curl.sh tag/create '{"name":"tag_name","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh tag/create '{"name":"tag1","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh tag/create '{"name":"tag2","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh tag/create '{"name":"tag4","token":"97b0febcad13268a5a12de9d09436ab5"}'

: create tag - already exist
./curl.sh tag/create '{"name":"tag_name","token":"97b0febcad13268a5a12de9d09436ab5"}'

: create tag - not admin
./curl.sh tag/create '{"name":"tag21","token":"8e37ca708c492c66383247ae57009531"}'
