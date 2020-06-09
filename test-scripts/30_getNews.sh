#!/bin/bash
set -x 

./curl.sh news/get '{"page":1,"created_after":"2020-01-04"}'