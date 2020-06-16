#!/bin/bash
set -x 

: delete news photo
./curl.sh news/deletephoto '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531","photo_id":1}'

: delete news photo - photo not exist
./curl.sh news/deletephoto '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531","photo_id":999999}'

: delete news photo - not author
./curl.sh news/deletephoto '{"news_id":1,"token":"036779522d916996be6944f885ce1af5","photo_id":1}'

: delete news photo - news not exist
./curl.sh news/deletephoto '{"news_id":999999,"token":"8e37ca708c492c66383247ae57009531","photo_id":1}'

: delete news photo - someone else_s news
./curl.sh news/deletephoto '{"news_id":1,"token":"b185b4ee226a682ff359b7640212c1c5","photo_id":1}'
