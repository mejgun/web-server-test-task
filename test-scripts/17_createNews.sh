#!/bin/bash
set -x 

./curl.sh createnews '{"name":"asd1","cat_id":1,"text":"some_text1","token":"1"}'
./curl.sh createnews '{"name":"asd2","cat_id":1,"text":"some_text1","token":"1"}'
./curl.sh createnews '{"name":"adasd","cat_id":1,"text":"some_text1","token":"8e37ca708c492c66383247ae57009531"}'
./curl.sh createnews '{"name":"asd3","cat_id":1,"text":"some_text1","token":"1"}'
