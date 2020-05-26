#!/bin/sh

curl -s -w "%{http_code}\n" -o - $@
echo
