#!/bin/bash
set -x 

: create news
./curl.sh news/create '{"name":"adasd","cat_id":1,"text":"some_text1","token":"8e37ca708c492c66383247ae57009531"}'
./curl.sh news/create '{"name":"zxcvzxcv","cat_id":2,"text":"text2","token":"8e37ca708c492c66383247ae57009531"}'

: create news - user not author
./curl.sh news/create '{"name":"asd1","cat_id":1,"text":"some_text1","token":"036779522d916996be6944f885ce1af5"}'

: create news - category not exist
./curl.sh news/create '{"name":"asd2","cat_id":999999,"text":"some_text1","token":"8e37ca708c492c66383247ae57009531"}'
