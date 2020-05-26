#!/bin/bash
set -x 

./curl.sh http://127.0.0.1:8080/createuser --data '{"name":"asd","1lastname":"dfd","login":"login1","password":"pass"}'
./curl.sh http://127.0.0.1:8080/createuser --data '{"name":"asd","lastname":"dfd","login":"login2","password":"pass"}'
./curl.sh http://127.0.0.1:8080/createuser --data '{"name":"asd","lastname":"dfd","photo":"12","login":"login3","password":"pass"}'
