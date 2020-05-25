#!/bin/bash
set -x 
curl http://127.0.0.1:8080/createuser --data '{"name":"asd","1lastname":"dfd"}' -o -
curl http://127.0.0.1:8080/createuser --data '{"name":"asd","lastname":"dfd"}' -o -
curl http://127.0.0.1:8080/createuser --data '{"name":"asd","lastname":"dfd","photo":"12"}' -o -
