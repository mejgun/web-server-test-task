#!/bin/bash
set -x 

: make author
./curl.sh author/make '{"login":"authorlogin","descr":"description","token":"97b0febcad13268a5a12de9d09436ab5"}'
./curl.sh author/make '{"login":"asdlogin","descr":"author_description","token":"97b0febcad13268a5a12de9d09436ab5"}'

: make author - user not exist
./curl.sh author/make '{"login":"login3","descr":"description","token":"97b0febcad13268a5a12de9d09436ab5"}'

: make author - user not admin
./curl.sh author/make '{"login":"asdlogin","descr":"author_description","token":"8e37ca708c492c66383247ae57009531"}'
