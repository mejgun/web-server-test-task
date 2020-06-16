#!/bin/bash
set -x 

: get drafts
./curl.sh news/getdrafts '{"page":1,"token":"8e37ca708c492c66383247ae57009531"}'

: get drafts - bad page
./curl.sh news/getdrafts '{"page":-1,"token":"8e37ca708c492c66383247ae57009531"}'

: get drafts - not author
./curl.sh news/getdrafts '{"page":1,"token":"036779522d916996be6944f885ce1af5"}'
