# API

## Endpoints

HTTP Method | API Method | Body (JSON) | Access
---|---|---|---
POST | createuser | name, lastname, login, password, photo (optional) | All
GET | getusers/page | - | All
POST | deleteuser | login, token | Admin
POST | loginuser | login, password | All