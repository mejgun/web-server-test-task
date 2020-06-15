#!/bin/bash
set -x 

: create category
./curl.sh category/create '{"name":"top_cat","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh category/create '{"name":"top_cat2","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh category/create '{"name":"cat1","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
./curl.sh category/create '{"name":"cat2","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
./curl.sh category/create '{"name":"cat3","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
./curl.sh category/create '{"name":"cat4","token":"97b0febcad13268a5a12de9d09436ab5","parent":2}'
./curl.sh category/create '{"name":"cat5","token":"97b0febcad13268a5a12de9d09436ab5","parent":2}'
./curl.sh category/create '{"name":"cat6","token":"97b0febcad13268a5a12de9d09436ab5","parent":2}'

: create category - user not admin
./curl.sh category/create '{"name":"cat21","token":"8e37ca708c492c66383247ae57009531","parent":2}'
