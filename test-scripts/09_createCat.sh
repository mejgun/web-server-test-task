#!/bin/bash
set -x 

./curl.sh createcategory '{"name":"main_cat","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh createcategory '{"name":"cat1","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
./curl.sh createcategory '{"name":"cat2","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
./curl.sh createcategory '{"name":"cat3","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
./curl.sh createcategory '{"name":"cat21","token":"97b0febcad13268a5a12de9d09436ab5","parent":2}'
