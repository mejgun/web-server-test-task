#!/bin/bash
set -x 

: news add comment
./curl.sh news/addcomment '{"news_id":2,"token":"036779522d916996be6944f885ce1af5","text":"comment_text_to_be_deleted"}'

: news add comment
./curl.sh news/addcomment '{"news_id":2,"token":"8e37ca708c492c66383247ae57009531","text":"test_comment_text"}'

: news add comment
./curl.sh news/addcomment '{"news_id":2,"token":"b185b4ee226a682ff359b7640212c1c5","text":"test_comment_text"}'

: news add comment - user not found
./curl.sh news/addcomment '{"news_id":1,"token":"0","text":"test_comment_text"}'

: news add comment - news not found
./curl.sh news/addcomment '{"news_id":999999,"token":"0","text":"test_comment_text"}'

: news add comment - news not published
./curl.sh news/addcomment '{"news_id":3,"token":"0","text":"test_comment_text"}'
