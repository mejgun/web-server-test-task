# web-server-test-task

## Config
### `pgconfig.txt`

text file with libpq connection string ([more](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING))


## Create DB structure

```
stack runhaskell db-scripts/CreateDB.hs
```

## Test scripts
- Insert some data
  ```
  stack runhaskell test-scripts/InsertTestData.hs
  ```
- Run shell scripts from `test-scripts` or run all scripts
  ```
  cd test-scripts && ls -1 ./*_*.sh|bash
  ```
  
## API

[API.md](https://github.com/mejgun/web-server-test-task/blob/master/API.md)
