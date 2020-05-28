#!/bin/bash
set -x 

./curl.sh createtag '{"name":"tag_name","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh createtag '{"name":"tag1","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh createtag '{"name":"tag2","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh createtag '{"name":"tag4","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh createtag '{"name":"tag21","token":"97b0febcad13268a5a12de9d09436ab5"}'
