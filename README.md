# web-server-test-task

## Config
### `config.json`

JSON type config

fields:
- pgconfig: libpq connection string ([more](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING))
- log_level: logging level (debug, normal, quiet)
- log_file: log file name

## Create DB structure

```
stack runhaskell -- -isrc db-scripts/CreateDB.hs
```

## Test scripts
- Insert some data
  ```
  stack runhaskell -- -isrc test-scripts/InsertTestData.hs
  ```
- Run shell scripts from `test-scripts` or run all scripts
  ```
  cd test-scripts && ls -1 ./*_*.sh|bash
  ```
  
## API

[API.md](https://github.com/mejgun/web-server-test-task/blob/master/API.md)
