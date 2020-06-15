#!/bin/bash
set -x 

: edit category 
./curl.sh category/edit '{"cat_id":1,"name":"renamed_top_cat","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh category/edit '{"cat_id":2,"name":"renamed_cat1","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
./curl.sh category/edit '{"cat_id":3,"name":"renamed_cat2","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'

: edit category - category not exist
./curl.sh category/edit '{"cat_id":999999,"name":"renamed_cat2","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'

: edit category - user not admin
./curl.sh category/edit '{"cat_id":4,"name":"renamed_cat2","token":"8e37ca708c492c66383247ae57009531","parent":1}'
