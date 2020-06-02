#!/bin/bash
set -x 

./curl.sh news/create '{"name":"asd1","cat_id":1,"text":"some_text1","token":"1"}'
./curl.sh news/create '{"name":"asd2","cat_id":1,"text":"some_text1","token":"1"}'
./curl.sh news/create '{"name":"adasd","cat_id":1,"text":"some_text1","token":"8e37ca708c492c66383247ae57009531"}'
./curl.sh news/create '{"name":"asd3","cat_id":1,"text":"some_text1","token":"1"}'
