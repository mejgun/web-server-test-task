# API

## Endpoints

HTTP method always POST

### Users

API Method | Body (JSON) | Access
---|---|---
createuser | name: string<br>lastname: string<br>login: string<br>password: string<br>photo: base64 encoded photo (optional)<br>photo_type: string (photo file type (extension), optional) | All
getusers | page: int | All
deleteuser | login: string<br>token: string | Admin
loginuser | login: string<br>password: string | All

### Authors

API Method | Body (JSON) | Access
---|---|---
makeauthor | login: string<br>description: string<br>token: string | Admin
editauthor | login: string<br>description: string<br>token: string | Admin
deleteauthor | login: string<br>token: string | Admin
getauthors | token: string<br>page: int | Admin

### Categories

API Method | Body (JSON) | Access
---|---|---
createcategory | name: string<br>parent: int (optional)<br>token: string | Admin
editcategory | cat_id: int<br>name: string<br>parent: int (optional)<br>token: string | Admin
deletecategory | cat_id: int<br>token: string | Admin
getcategories | page: int | All

### Tags

API Method | Body (JSON) | Access
---|---|---
createtag | name: string<br>token: string | Admin
edittag | tag_id: int<br>name: string<br>token: string | Admin
deletetag | tag_id: int<br>token: string | Admin
gettags | page: int | All

### News & Drafts

API Method | Body (JSON) | Access
---|---|---
createnews | name: string<br>cat_id: int<br>text: string<br>token: string | Author
updatenews | news_id: int<br>name: string<br>cat_id: int<br>text: string<br>token: string | Author
publishnews | news_id: int<br>publish: bool<br>token: string | Author
setnewsmainphoto | news_id: int<br>photo: base64 encoded photo<br>photo_type: string (photo file type (extension), optional)<br>token: string | Author
