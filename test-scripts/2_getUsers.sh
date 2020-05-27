#!/bin/bash
set -x 

./curl.sh http://127.0.0.1:8080/getusers
./curl.sh http://127.0.0.1:8080/getusers/1
./curl.sh http://127.0.0.1:8080/getusers/d
