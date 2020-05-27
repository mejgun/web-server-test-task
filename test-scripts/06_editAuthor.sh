#!/bin/bash
set -x 

./curl.sh http://127.0.0.1:8080/editauthor --data '{"login":"authorlogin","descr":"fixeddescription","token":"97b0febcad13268a5a12de9d09436ab5"}'
