#!/bin/bash
set -x 

: delete news comment 
./curl.sh news/deletecomment '{"comment_id":1,"token":"97b0febcad13268a5a12de9d09436ab5"}'

: delete news comment - not admin
./curl.sh news/deletecomment '{"comment_id":1,"token":"036779522d916996be6944f885ce1af5"}'

: delete news comment - comment not exist
./curl.sh news/deletecomment '{"comment_id":1,"token":"97b0febcad13268a5a12de9d09436ab5"}'