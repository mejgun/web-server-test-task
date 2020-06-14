#!/bin/bash
set -x 

: user login
./curl.sh user/login '{"login":"user","password":"userpass"}'

: user login - bad password
./curl.sh user/login '{"login":"asdlogin","password":"wrongpass"}'

: user login - bad login
./curl.sh user/login '{"login":"u","password":"pass"}'
