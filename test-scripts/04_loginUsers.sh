#!/bin/bash
set -x 

./curl.sh loginuser '{"login":"user","password":"userpass"}'
./curl.sh loginuser '{"login":"user","password":"wrongpass"}'
