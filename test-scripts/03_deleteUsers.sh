#!/bin/bash
set -x 

./curl.sh deleteuser '{"login":"login3","token":"036779522d916996be6944f885ce1af5"}' 
./curl.sh deleteuser '{"login":"login3","token":"036779522d916996be6944f885ce1af5"}' 
./curl.sh deleteuser '{"login":"login2","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh deleteuser '{"login":"login2","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh deleteuser '{"login":"login3","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh deleteuser '{"login":"login3","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh deleteuser '{"login":"nologin","token":"97b0febcad13268a5a12de9d09436ab5"}'
