# API

## Endpoints

### Users

HTTP Method | API Method | Body (JSON) | Access
---|---|---|---
POST | createuser | name, lastname, login, password, photo (optional) | All
GET | getusers/page | - | All
POST | deleteuser | login, token | Admin
POST | loginuser | login, password | All

### Authors

HTTP Method | API Method | Body (JSON) | Access
---|---|---|---
POST | makeauthor | login, description, token | Admin
POST | editauthor | login, description, token | Admin
POST | deleteauthor | login, token | Admin
POST | getauthors/page | token | Admin