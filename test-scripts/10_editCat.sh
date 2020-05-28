#!/bin/bash
set -x 

./curl.sh editcategory '{"cat_id":2,"name":"renamed_main_cat","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh editcategory '{"cat_id":3,"name":"renamed_cat1","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
./curl.sh editcategory '{"cat_id":4,"name":"renamed_cat2","token":"97b0febcad13268a5a12de9d09436ab5","parent":1}'
