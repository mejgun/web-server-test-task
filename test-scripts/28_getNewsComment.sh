#!/bin/bash
set -x 

: get news comments
./curl.sh news/getcomments '{"news_id":2,"page":1}'

: get news comments - news not exist
./curl.sh news/getcomments '{"news_id":9999,"page":1}'

: get news comments - bad page
./curl.sh news/getcomments '{"news_id":1,"page":-1}'
