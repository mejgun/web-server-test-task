#!/bin/bash
set -x 

./curl.sh user/login '{"login":"user","password":"userpass"}'
./curl.sh user/login '{"login":"user","password":"wrongpass"}'
