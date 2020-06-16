#!/bin/bash
set -x 

: delete news tag
./curl.sh news/deletetag '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531","tag_id":2}'

: delete news tag  - tag not exist
./curl.sh news/deletetag '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531","tag_id":9999999}'

: delete news tag  - not author
./curl.sh news/deletetag '{"news_id":1,"token":"036779522d916996be6944f885ce1af5","tag_id":1}'

: delete news tag  - news not exist
./curl.sh news/deletetag '{"news_id":999999,"token":"8e37ca708c492c66383247ae57009531","tag_id":1}'

: delete news tag  - someone else_s news
./curl.sh news/deletetag '{"news_id":1,"token":"b185b4ee226a682ff359b7640212c1c5","tag_id":1}'
