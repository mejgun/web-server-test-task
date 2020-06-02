#!/bin/bash
set -x 

./curl.sh author/make '{"login":"authorlogin","descr":"description","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh author/make '{"login":"login3","descr":"description","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh author/make '{"login":"testauthoruser","descr":"author_description","token":"97b0febcad13268a5a12de9d09436ab5"}'
