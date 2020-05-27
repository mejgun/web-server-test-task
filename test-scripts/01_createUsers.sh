#!/bin/bash
set -x 

./curl.sh createuser '{"name":"asd","1lastname":"dfd","login":"login1","password":"pass"}'
./curl.sh createuser '{"name":"asd","lastname":"dfd","login":"login2","password":"pass"}'
./curl.sh createuser '{"name":"authorname","lastname":"authorlastname","login":"authorlogin","password":"pass"}'
./curl.sh createuser '{"name":"asd","lastname":"dfd","photo":"12","login":"login3","password":"pass"}'
