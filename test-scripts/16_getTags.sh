#!/bin/bash
set -x 

: get tags
./curl.sh tag/get '{"page":1}'

: get tags - bad page
./curl.sh tag/get '{"page":-1}'
