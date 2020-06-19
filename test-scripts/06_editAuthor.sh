#!/bin/bash
set -x 

: edit author
./curl.sh author/edit '{"login":"author2","descr":"fixeddescription","token":"97b0febcad13268a5a12de9d09436ab5"}'

: edit author - author not exist
./curl.sh author/edit '{"login":"login","descr":"fixeddescription","token":"97b0febcad13268a5a12de9d09436ab5"}'

: edit author - user not author
./curl.sh author/edit '{"login":"testuser","descr":"fixeddescription","token":"97b0febcad13268a5a12de9d09436ab5"}'

: edit author - user not admin
./curl.sh author/edit '{"login":"testuser","descr":"fixeddescription","token":"8e37ca708c492c66383247ae57009531"}'
