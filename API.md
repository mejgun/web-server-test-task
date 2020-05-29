# API

## Endpoints

HTTP method always POST

### Users

API Method | Body (JSON) | Access
---|---|---
createuser | name: string<br>lastname: string<br>login: string<br>password: string<br>photo: base64 encoded photo (optional)<br>photo_type: string (photo file type (extension), optional) | All
getusers | page: string | All
deleteuser | login: string<br>token: string | Admin
loginuser | login: string<br>password: string | All

### Authors

API Method | Body (JSON) | Access
---|---|---
makeauthor | login, description, token | Admin
editauthor | login, description, token | Admin
deleteauthor | login, token | Admin
getauthors | token, page | Admin

### Categories

API Method | Body (JSON) | Access
---|---|---
createcategory | name, parent (optional), token | Admin
editcategory | cat_id, name, parent (optional), token | Admin
deletecategory | cat_id, token | Admin
getcategories | page | All

### Tags

API Method | Body (JSON) | Access
---|---|---
createtag | name, token | Admin
edittag | tag_id, name, token | Admin
deletetag | tag_id, token | Admin
gettags | page | All

### Drafts

### News