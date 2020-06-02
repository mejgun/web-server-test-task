#!/bin/bash
set -x 

./curl.sh news/addcomment '{"news_id":1,"token":"036779522d916996be6944f885ce1af5","text":"comment_text_to_be_deleted"}'
./curl.sh news/addcomment '{"news_id":1,"token":"036779522d916996be6944f885ce1af5","text":"test_comment_text"}'
./curl.sh news/addcomment '{"news_id":1,"token":"036779522d916996be6944f885ce1af5","text":"test_comment_text"}'
