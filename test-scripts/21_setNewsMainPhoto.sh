#!/bin/bash
set -x 

: set news main photo
./curl.sh news/setmainphoto '{"news_id":1,"token":"8e37ca708c492c66383247ae57009531","photo_type":"png","photo":"iVBORw0KGgoAAAANSUhEUgAAAIAAAACACAYAAADDPmHLAAAACXBIWXMAAAsSAAALEgHS3X78AAACo0lEQVR42u3crYtUURgH4FEYNVhEULYoiNa1rGgS4c5lDWbBP8CszaIw6fqRBbNZ2CqYxG5zNRkMlnWLwSB+voMbZDhT9sz92DnPA2+ZcJjhHH6/uXNnZjQCAAAAAAAAAAAAAAAAAAAAoAyTyWQj5nvMr33OdsyxuTUPx7zOWHM2N+xOd4fgYcyfjHmSWPNCzLeMNT/HnLA73RyAozHvMjbrZ8yVxLp3Mg/Wc7vT3SG4FPMjY7M+zFdBXdezKniTeQhUgSpQBaoAVWB3VIEqUAUclCp4rwpUwWNVoApUgSpQBapAFagCVaAKVIEqUAWqQBWogv/XrKqqlSqIx47EXBvQnFMFHVbBdDqdrbuV+VyXOY0q6LgK4rHTMbsOQNlVcMsBUAVbDoAq2HUAVIEDoAocAFXgAKgCB0AVOAAtVMF2ZhVcTqx7t6UquNfxXHWvYEBVgNvGtHQAct9kvUyseTzmk9vGw9/8UzE7GZv0NeZMYt2nS3ojpgpaPgAvMjfodmLN2b3030s6AMkqiMdOxlxsedZWffNvZm7Oq7quDyWi/+OSL8cWXRV8cRm4utE/hA+IGtHfX/QP4QOiRvT3G/193ytoRH//0d9nFTSiv//o77MKGtE/jOjvqwoa0T+c6O+jChrRP5zo76MKGtHfXvTP/u/42cCroBH97UX/g70/qW7jG0R1zKMlzKbobyf631ZVNd5b67zbxuVF//rcmn5tXFL0J9b0DaLSoj+xtiooLfoTz1sVlBT9qkD0qwLRrwqKj35VsMLRHzPe5+tRBaVFvyooPPpVgehPvT4/MSst+lVB4dE/z6+NC4x+VSD6VcEBi/77bb92VfBvo87GXM+Y9QWXW5uZ6447ev1rmc9zYwQAAAAAAAAAAAAAAAAAALDQXyWin4ktTe7TAAAAAElFTkSuQmCC"}'
./curl.sh news/setmainphoto '{"news_id":2,"token":"8e37ca708c492c66383247ae57009531","photo_type":"png","photo":"iVBORw0KGgoAAAANSUhEUgAAAIAAAACACAYAAADDPmHLAAAACXBIWXMAAAsSAAALEgHS3X78AAACo0lEQVR42u3crYtUURgH4FEYNVhEULYoiNa1rGgS4c5lDWbBP8CszaIw6fqRBbNZ2CqYxG5zNRkMlnWLwSB+voMbZDhT9sz92DnPA2+ZcJjhHH6/uXNnZjQCAAAAAAAAAAAAAAAAAAAAoAyTyWQj5nvMr33OdsyxuTUPx7zOWHM2N+xOd4fgYcyfjHmSWPNCzLeMNT/HnLA73RyAozHvMjbrZ8yVxLp3Mg/Wc7vT3SG4FPMjY7M+zFdBXdezKniTeQhUgSpQBaoAVWB3VIEqUAUclCp4rwpUwWNVoApUgSpQBapAFagCVaAKVIEqUAWqQBWogv/XrKqqlSqIx47EXBvQnFMFHVbBdDqdrbuV+VyXOY0q6LgK4rHTMbsOQNlVcMsBUAVbDoAq2HUAVIEDoAocAFXgAKgCB0AVOAAtVMF2ZhVcTqx7t6UquNfxXHWvYEBVgNvGtHQAct9kvUyseTzmk9vGw9/8UzE7GZv0NeZMYt2nS3ojpgpaPgAvMjfodmLN2b3030s6AMkqiMdOxlxsedZWffNvZm7Oq7quDyWi/+OSL8cWXRV8cRm4utE/hA+IGtHfX/QP4QOiRvT3G/193ytoRH//0d9nFTSiv//o77MKGtE/jOjvqwoa0T+c6O+jChrRP5zo76MKGtHfXvTP/u/42cCroBH97UX/g70/qW7jG0R1zKMlzKbobyf631ZVNd5b67zbxuVF//rcmn5tXFL0J9b0DaLSoj+xtiooLfoTz1sVlBT9qkD0qwLRrwqKj35VsMLRHzPe5+tRBaVFvyooPPpVgehPvT4/MSst+lVB4dE/z6+NC4x+VSD6VcEBi/77bb92VfBvo87GXM+Y9QWXW5uZ6447ev1rmc9zYwQAAAAAAAAAAAAAAAAAALDQXyWin4ktTe7TAAAAAElFTkSuQmCC"}'

: set news main photo - not author
./curl.sh news/setmainphoto '{"news_id":1,"token":"036779522d916996be6944f885ce1af5","photo_type":"png","photo":"i"}'

: set news main photo - news not exist
./curl.sh news/setmainphoto '{"news_id":999999,"token":"8e37ca708c492c66383247ae57009531","photo_type":"png","photo":"i"}'

: set news main photo - someone else_s news
./curl.sh news/setmainphoto '{"news_id":1,"token":"b185b4ee226a682ff359b7640212c1c5","photo_type":"png","photo":"i"}'
