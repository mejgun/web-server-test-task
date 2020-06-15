#!/bin/bash
set -x 

: update news
./curl.sh news/update '{"news_id":1,"name":"rename_adasd","cat_id":2,"text":"edited__text","token":"8e37ca708c492c66383247ae57009531"}'
./curl.sh news/update '{"news_id":2,"name":"renamed_news_name","cat_id":1,"text":"text","token":"8e37ca708c492c66383247ae57009531"}'

: update news - not author
./curl.sh news/update '{"news_id":1,"name":"a","cat_id":1,"text":"t","token":"036779522d916996be6944f885ce1af5"}'

: update news - news not exist
./curl.sh news/update '{"news_id":999999,"name":"rename_adasd","cat_id":2,"text":"edited__text","token":"8e37ca708c492c66383247ae57009531"}'

: update news - category not exist
./curl.sh news/update '{"news_id":1,"name":"rename_adasd","cat_id":999999,"text":"edited__text","token":"8e37ca708c492c66383247ae57009531"}'

: update news - someone else_s news
./curl.sh news/update '{"news_id":1,"name":"a","cat_id":1,"text":"t","token":"b185b4ee226a682ff359b7640212c1c5"}'
