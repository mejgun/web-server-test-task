#!/bin/bash
set -x 

: get categories
./curl.sh category/get '{"page":1}'

: get categories - bad page
./curl.sh category/get '{"page":-4}'
