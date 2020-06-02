#!/bin/bash
set -x 

./curl.sh news/update '{"name":"asd2","cat_id":1,"text":"some_text1","token":"1"}'
./curl.sh news/update '{"news_id":1,"name":"rename_adasd","cat_id":2,"text":"edited__text","token":"8e37ca708c492c66383247ae57009531"}'
