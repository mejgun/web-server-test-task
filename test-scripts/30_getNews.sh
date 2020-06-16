#!/bin/bash
set -x 

: get news
./curl.sh news/get '{"page":1,"created_after":"2020-01-01","created_before":"2021-01-01","author_contains":"user","name_contains":"name","text_contains":"text","anything_contains":"lastname","cat_id":1,"tags_all":[1,2],"tags_any":[2,4],"sort_by":"photos"}'

: get news
./curl.sh news/get '{"page":-1}'
