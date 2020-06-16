#!/bin/bash
set -x 

: publish news
./curl.sh news/publish '{"news_id":1,"publish":true,"token":"8e37ca708c492c66383247ae57009531"}'
./curl.sh news/publish '{"news_id":2,"publish":true,"token":"8e37ca708c492c66383247ae57009531"}'

: publish news - someone else_s news
./curl.sh news/publish '{"news_id":2,"publish":true,"token":"b185b4ee226a682ff359b7640212c1c5"}'

: publish news - not an author
./curl.sh news/publish '{"news_id":1,"publish":true,"token":"036779522d916996be6944f885ce1af5"}'

: publish news - news not exist
./curl.sh news/publish '{"news_id":999999,"publish":true,"token":"8e37ca708c492c66383247ae57009531"}'
