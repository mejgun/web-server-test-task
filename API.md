# API

## Endpoints

HTTP Method | API Method | Body (JSON) | Admin
---|---|---|---
POST | createuser | name, lastname, login, password, photo (optional) | False
GET | getusers | - | False
POST | deleteuser | login, token | True