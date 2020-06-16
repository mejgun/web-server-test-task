#!/bin/bash
set -x 

: news add tag 
./curl.sh news/addtag '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531","tag_id":1}'
./curl.sh news/addtag '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531","tag_id":2}'
./curl.sh news/addtag '{"news_id":2,"token":"8e37ca708c492c66383247ae57009531","tag_id":2}'
./curl.sh news/addtag '{"news_id":2,"token":"8e37ca708c492c66383247ae57009531","tag_id":1}'

: news add tag  - tag not exist
./curl.sh news/addtag '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531","tag_id":999999}'

: news add tag  - not author
./curl.sh news/addtag '{"news_id":1,"token":"036779522d916996be6944f885ce1af5","tag_id":1}'

: news add tag  - news not exist
./curl.sh news/addtag '{"news_id":999999,"token":"8e37ca708c492c66383247ae57009531","tag_id":1}'

: news add tag  - someone else_s news
./curl.sh news/addtag '{"news_id":1,"token":"b185b4ee226a682ff359b7640212c1c5","tag_id":1}'
