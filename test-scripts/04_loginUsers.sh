#!/bin/bash
set -x 

./curl.sh http://127.0.0.1:8080/loginuser --data '{"login":"user","password":"userpass"}'
./curl.sh http://127.0.0.1:8080/loginuser --data '{"login":"user","password":"wrongpass"}'
