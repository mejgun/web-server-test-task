#!/bin/bash
set -x 

./curl.sh http://127.0.0.1:8080/makeauthor --data '{"login":"authorlogin","descr":"description","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh http://127.0.0.1:8080/makeauthor --data '{"login":"login3","descr":"description","token":"97b0febcad13268a5a12de9d09436ab5"}'
