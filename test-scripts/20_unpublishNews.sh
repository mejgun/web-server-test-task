#!/bin/bash
set -x 

: unpublish news
./curl.sh news/publish '{"news_id":1,"publish":false,"token":"8e37ca708c492c66383247ae57009531"}'

: unpublish news - someone else_s news
./curl.sh news/publish '{"news_id":2,"publish":false,"token":"b185b4ee226a682ff359b7640212c1c5"}'

: unpublish news - not an author
./curl.sh news/publish '{"news_id":1,"publish":false,"token":"036779522d916996be6944f885ce1af5"}'

: unpublish news - news not exist
./curl.sh news/publish '{"news_id":999999,"publish":false,"token":"8e37ca708c492c66383247ae57009531"}'