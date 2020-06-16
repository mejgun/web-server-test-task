#!/bin/bash
set -x 

: delete news
./curl.sh news/delete '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531"}'

: delete news - news not found
./curl.sh news/delete '{"news_id":9999999,"token":"8e37ca708c492c66383247ae57009531"}'

: delete news - someone else_s news
./curl.sh news/delete '{"news_id":2,"token":"b185b4ee226a682ff359b7640212c1c5"}'

: delete news - not author
./curl.sh news/delete '{"news_id":2,"token":"036779522d916996be6944f885ce1af5"}'

