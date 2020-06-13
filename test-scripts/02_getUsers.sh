#!/bin/bash
set -x 

: get users
./curl.sh user/get '{"page":1}'

: get users - bad page
./curl.sh user/get '{"page":-1}'

: get users - bad json
./curl.sh user/get '{"page":"1"}'