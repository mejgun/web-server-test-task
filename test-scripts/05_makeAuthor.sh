#!/bin/bash
set -x 

./curl.sh makeauthor '{"login":"authorlogin","descr":"description","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh makeauthor '{"login":"login3","descr":"description","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh makeauthor '{"login":"testauthoruser","descr":"author_description","token":"97b0febcad13268a5a12de9d09436ab5"}'
