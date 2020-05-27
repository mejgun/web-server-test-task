#!/bin/sh

curl -s -w " %{http_code}\n" -o - http://127.0.0.1:8080/"$1" --data "$2"
echo
